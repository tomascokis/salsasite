import { randomUUID } from 'node:crypto';
import type { RequestEvent } from '@sveltejs/kit';
import { getAppDatabase, runInTransaction } from './app-state';

const PAGE_WARNING_PER_HOUR = 120;
const VIDEO_WARNING_PER_HOUR = 40;
const WEEKLY_IP_WARNING = 3;
const FAILED_LOGIN_WARNING_PER_HOUR = 10;
const RECENT_ATTEMPT_LIMIT = 50;

export type UsageKind = 'page' | 'video';

export type SecurityDashboardUser = {
  userId: string;
  username: string;
  role: 'viewer' | 'admin';
  pagesThisHour: number;
  videosThisHour: number;
  pagesLast24Hours: number;
  videosLast24Hours: number;
  uniqueIpsThisWeek: number;
  warningReasons: string[];
};

export type SecurityDashboard = {
  generatedAt: string;
  thresholds: {
    pagesPerHour: number;
    videosPerHour: number;
    weeklyIpsPerAccount: number;
    failedLoginsPerHour: number;
  };
  users: SecurityDashboardUser[];
  loginViewsByIp: Array<{
    ipAddress: string;
    viewsThisHour: number;
    viewsLast24Hours: number;
  }>;
  failedAttemptsByIp: Array<{
    ipAddress: string;
    attemptsThisHour: number;
    warning: boolean;
  }>;
  failedAttemptsByUsername: Array<{
    username: string;
    attemptsThisHour: number;
    warning: boolean;
  }>;
  successfulAttemptsByUsername: Array<{
    username: string;
    attemptsThisHour: number;
  }>;
  recentAttempts: Array<{
    createdAt: string;
    username: string;
    ipAddress: string;
    success: boolean;
  }>;
};

type CountRow = Record<string, unknown>;

function nowIso() {
  return new Date().toISOString();
}

function normalizeUsername(username: unknown) {
  return String(username ?? '').replace(/\s+/g, ' ').trim().toLocaleLowerCase();
}

export function hourBucket(value: Date | string = new Date()) {
  const date = value instanceof Date ? value : new Date(value);
  const bucket = new Date(date);
  bucket.setUTCMinutes(0, 0, 0);
  return bucket.toISOString();
}

export function weekBucket(value: Date | string = new Date()) {
  const date = value instanceof Date ? value : new Date(value);
  const bucket = new Date(Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), date.getUTCDate()));
  const day = bucket.getUTCDay();
  const mondayOffset = day === 0 ? -6 : 1 - day;
  bucket.setUTCDate(bucket.getUTCDate() + mondayOffset);
  return bucket.toISOString().slice(0, 10);
}

function isoHoursAgo(hours: number, from: Date) {
  return hourBucket(new Date(from.getTime() - hours * 60 * 60 * 1000));
}

function normalizeIpAddress(value: unknown) {
  const raw = String(value ?? '').trim();
  if (!raw || raw.length > 128 || /\s/.test(raw)) {
    return 'unknown';
  }
  return raw.replace(/^\[|\]$/g, '');
}

function forwardedIp(headers: Headers) {
  const forwardedFor = headers.get('x-forwarded-for');
  if (forwardedFor) {
    const [first] = forwardedFor.split(',');
    if (first?.trim()) {
      return normalizeIpAddress(first);
    }
  }

  const realIp = headers.get('x-real-ip');
  return realIp ? normalizeIpAddress(realIp) : null;
}

export function clientIpFromEvent(event: Pick<RequestEvent, 'request' | 'getClientAddress'>) {
  if (process.env.TRUST_PROXY_HEADERS === 'true') {
    const proxyIp = forwardedIp(event.request.headers);
    if (proxyIp) {
      return proxyIp;
    }
  }

  try {
    return normalizeIpAddress(event.getClientAddress());
  } catch {
    return 'unknown';
  }
}

export function recordAuthenticatedUsage(input: {
  userId: string;
  kind: UsageKind;
  ipAddress: string;
  at?: Date | string;
}) {
  const at = input.at ? new Date(input.at) : new Date();
  const timestamp = at.toISOString();
  const hour = hourBucket(at);
  const week = weekBucket(at);
  const pageIncrement = input.kind === 'page' ? 1 : 0;
  const videoIncrement = input.kind === 'video' ? 1 : 0;
  const ipAddress = normalizeIpAddress(input.ipAddress);

  runInTransaction((db) => {
    db.prepare(
      `
        INSERT INTO security_user_usage_hourly (
          user_id, hour_bucket, page_count, video_count, updated_at
        ) VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(user_id, hour_bucket) DO UPDATE SET
          page_count = page_count + excluded.page_count,
          video_count = video_count + excluded.video_count,
          updated_at = excluded.updated_at
      `
    ).run(input.userId, hour, pageIncrement, videoIncrement, timestamp);

    db.prepare(
      `
        INSERT INTO security_user_weekly_ips (
          user_id, week_bucket, ip_address, first_seen_at, last_seen_at, request_count
        ) VALUES (?, ?, ?, ?, ?, 1)
        ON CONFLICT(user_id, week_bucket, ip_address) DO UPDATE SET
          last_seen_at = excluded.last_seen_at,
          request_count = request_count + 1
      `
    ).run(input.userId, week, ipAddress, timestamp, timestamp);
  });
}

export function recordLoginView(input: { ipAddress: string; at?: Date | string }) {
  const at = input.at ? new Date(input.at) : new Date();
  const timestamp = at.toISOString();
  const hour = hourBucket(at);
  const ipAddress = normalizeIpAddress(input.ipAddress);

  getAppDatabase()
    .prepare(
      `
        INSERT INTO security_login_views_hourly (
          hour_bucket, ip_address, view_count, updated_at
        ) VALUES (?, ?, 1, ?)
        ON CONFLICT(hour_bucket, ip_address) DO UPDATE SET
          view_count = view_count + 1,
          updated_at = excluded.updated_at
      `
    )
    .run(hour, ipAddress, timestamp);
}

export function recordLoginAttempt(input: {
  username: string;
  userId?: string | null;
  ipAddress: string;
  success: boolean;
  at?: Date | string;
}) {
  const at = input.at ? new Date(input.at) : new Date();
  getAppDatabase()
    .prepare(
      `
        INSERT INTO security_login_attempts (
          id, attempted_username, attempted_username_normalized,
          user_id, ip_address, success, created_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?)
      `
    )
    .run(
      randomUUID(),
      String(input.username ?? '').trim(),
      normalizeUsername(input.username),
      input.userId ?? null,
      normalizeIpAddress(input.ipAddress),
      input.success ? 1 : 0,
      at.toISOString()
    );
}

function numberValue(value: unknown) {
  return Number(value ?? 0);
}

function warningReasonsForUser(user: SecurityDashboardUser) {
  const reasons: string[] = [];
  if (user.pagesThisHour > PAGE_WARNING_PER_HOUR) {
    reasons.push(`Pages this hour exceed ${PAGE_WARNING_PER_HOUR}.`);
  }
  if (user.videosThisHour > VIDEO_WARNING_PER_HOUR) {
    reasons.push(`Video requests this hour exceed ${VIDEO_WARNING_PER_HOUR}.`);
  }
  if (user.uniqueIpsThisWeek > WEEKLY_IP_WARNING) {
    reasons.push(`Unique IPs this week exceed ${WEEKLY_IP_WARNING}.`);
  }
  return reasons;
}

export function getSecurityDashboard(input: { now?: Date | string } = {}): SecurityDashboard {
  const now = input.now ? new Date(input.now) : new Date();
  const generatedAt = now.toISOString();
  const currentHour = hourBucket(now);
  const last24HourStart = isoHoursAgo(23, now);
  const currentWeek = weekBucket(now);
  const db = getAppDatabase();

  const users = db
    .prepare(
      `
        SELECT
          u.id AS user_id,
          u.username,
          u.role,
          COALESCE(current_usage.page_count, 0) AS pages_this_hour,
          COALESCE(current_usage.video_count, 0) AS videos_this_hour,
          COALESCE(day_usage.pages_last_24_hours, 0) AS pages_last_24_hours,
          COALESCE(day_usage.videos_last_24_hours, 0) AS videos_last_24_hours,
          COALESCE(weekly_ips.unique_ips_this_week, 0) AS unique_ips_this_week
        FROM auth_users u
        LEFT JOIN security_user_usage_hourly current_usage
          ON current_usage.user_id = u.id AND current_usage.hour_bucket = ?
        LEFT JOIN (
          SELECT
            user_id,
            SUM(page_count) AS pages_last_24_hours,
            SUM(video_count) AS videos_last_24_hours
          FROM security_user_usage_hourly
          WHERE hour_bucket >= ?
          GROUP BY user_id
        ) day_usage ON day_usage.user_id = u.id
        LEFT JOIN (
          SELECT user_id, COUNT(*) AS unique_ips_this_week
          FROM security_user_weekly_ips
          WHERE week_bucket = ?
          GROUP BY user_id
        ) weekly_ips ON weekly_ips.user_id = u.id
        WHERE u.is_active = 1
        ORDER BY u.username COLLATE NOCASE
      `
    )
    .all(currentHour, last24HourStart, currentWeek) as CountRow[];

  const dashboardUsers = users.map((row) => {
    const user: SecurityDashboardUser = {
      userId: String(row.user_id),
      username: String(row.username),
      role: row.role === 'admin' ? 'admin' : 'viewer',
      pagesThisHour: numberValue(row.pages_this_hour),
      videosThisHour: numberValue(row.videos_this_hour),
      pagesLast24Hours: numberValue(row.pages_last_24_hours),
      videosLast24Hours: numberValue(row.videos_last_24_hours),
      uniqueIpsThisWeek: numberValue(row.unique_ips_this_week),
      warningReasons: []
    };
    return {
      ...user,
      warningReasons: warningReasonsForUser(user)
    };
  });

  const loginViewsByIp = db
    .prepare(
      `
        SELECT
          ip_address,
          SUM(CASE WHEN hour_bucket = ? THEN view_count ELSE 0 END) AS views_this_hour,
          SUM(view_count) AS views_last_24_hours
        FROM security_login_views_hourly
        WHERE hour_bucket >= ?
        GROUP BY ip_address
        ORDER BY views_last_24_hours DESC, ip_address
        LIMIT 50
      `
    )
    .all(currentHour, last24HourStart) as CountRow[];

  const failedAttemptsByIp = db
    .prepare(
      `
        SELECT ip_address, COUNT(*) AS attempts_this_hour
        FROM security_login_attempts
        WHERE created_at >= ? AND success = 0
        GROUP BY ip_address
        ORDER BY attempts_this_hour DESC, ip_address
        LIMIT 50
      `
    )
    .all(currentHour) as CountRow[];

  const failedAttemptsByUsername = db
    .prepare(
      `
        SELECT attempted_username_normalized AS username, COUNT(*) AS attempts_this_hour
        FROM security_login_attempts
        WHERE created_at >= ? AND success = 0
        GROUP BY attempted_username_normalized
        ORDER BY attempts_this_hour DESC, username
        LIMIT 50
      `
    )
    .all(currentHour) as CountRow[];

  const successfulAttemptsByUsername = db
    .prepare(
      `
        SELECT attempted_username_normalized AS username, COUNT(*) AS attempts_this_hour
        FROM security_login_attempts
        WHERE created_at >= ? AND success = 1
        GROUP BY attempted_username_normalized
        ORDER BY attempts_this_hour DESC, username
        LIMIT 50
      `
    )
    .all(currentHour) as CountRow[];

  const recentAttempts = db
    .prepare(
      `
        SELECT attempted_username, ip_address, success, created_at
        FROM security_login_attempts
        ORDER BY created_at DESC
        LIMIT ?
      `
    )
    .all(RECENT_ATTEMPT_LIMIT) as CountRow[];

  return {
    generatedAt,
    thresholds: {
      pagesPerHour: PAGE_WARNING_PER_HOUR,
      videosPerHour: VIDEO_WARNING_PER_HOUR,
      weeklyIpsPerAccount: WEEKLY_IP_WARNING,
      failedLoginsPerHour: FAILED_LOGIN_WARNING_PER_HOUR
    },
    users: dashboardUsers,
    loginViewsByIp: loginViewsByIp.map((row) => ({
      ipAddress: String(row.ip_address),
      viewsThisHour: numberValue(row.views_this_hour),
      viewsLast24Hours: numberValue(row.views_last_24_hours)
    })),
    failedAttemptsByIp: failedAttemptsByIp.map((row) => {
      const attemptsThisHour = numberValue(row.attempts_this_hour);
      return {
        ipAddress: String(row.ip_address),
        attemptsThisHour,
        warning: attemptsThisHour > FAILED_LOGIN_WARNING_PER_HOUR
      };
    }),
    failedAttemptsByUsername: failedAttemptsByUsername.map((row) => {
      const attemptsThisHour = numberValue(row.attempts_this_hour);
      return {
        username: String(row.username || '(blank)'),
        attemptsThisHour,
        warning: attemptsThisHour > FAILED_LOGIN_WARNING_PER_HOUR
      };
    }),
    successfulAttemptsByUsername: successfulAttemptsByUsername.map((row) => ({
      username: String(row.username || '(blank)'),
      attemptsThisHour: numberValue(row.attempts_this_hour)
    })),
    recentAttempts: recentAttempts.map((row) => ({
      createdAt: String(row.created_at),
      username: String(row.attempted_username || '(blank)'),
      ipAddress: String(row.ip_address),
      success: Boolean(row.success)
    }))
  };
}
