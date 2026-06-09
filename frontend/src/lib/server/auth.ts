import {
  randomBytes,
  randomUUID,
  scrypt as scryptCallback,
  scryptSync,
  timingSafeEqual,
  createHash
} from 'node:crypto';
import { promisify } from 'node:util';
import { getAppDatabase, runInTransaction } from './app-state';

const scrypt = promisify(scryptCallback);
const SESSION_DAYS = 30;
const SESSION_COOKIE_NAME = 'salsa_session';
const SCRYPT_KEY_LENGTH = 64;
const SCRYPT_OPTIONS = { N: 16384, r: 8, p: 1 } as const;
const DEFAULT_ADMIN_USERNAME = 'tomascokis';
const DEFAULT_ADMIN_PASSWORD = 'superfollow';

export type AuthRole = 'viewer' | 'admin';

export type AuthUser = {
  id: string;
  username: string;
  role: AuthRole;
  isActive: boolean;
  createdAt: string;
  updatedAt: string;
  lastLoginAt: string | null;
};

type AuthUserRow = {
  id: string;
  username: string;
  role: string;
  is_active: number;
  created_at: string;
  updated_at: string;
  last_login_at: string | null;
};

type AuthUserWithPasswordRow = AuthUserRow & {
  password_hash: string;
};

export const sessionCookieName = SESSION_COOKIE_NAME;

function nowIso() {
  return new Date().toISOString();
}

function normalizeUsername(username: unknown) {
  return String(username ?? '').replace(/\s+/g, ' ').trim().toLocaleLowerCase();
}

function validateRole(role: unknown): AuthRole {
  if (role === 'viewer' || role === 'admin') {
    return role;
  }
  throw new Error('Role must be viewer or admin.');
}

function userFromRow(row: AuthUserRow): AuthUser {
  return {
    id: row.id,
    username: row.username,
    role: row.role === 'admin' ? 'admin' : 'viewer',
    isActive: Boolean(row.is_active),
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    lastLoginAt: row.last_login_at
  };
}

async function hashPassword(password: string) {
  const salt = randomBytes(16).toString('base64url');
  const derivedKey = (await scrypt(password, salt, SCRYPT_KEY_LENGTH, SCRYPT_OPTIONS)) as Buffer;
  return [
    'scrypt',
    String(SCRYPT_OPTIONS.N),
    String(SCRYPT_OPTIONS.r),
    String(SCRYPT_OPTIONS.p),
    salt,
    derivedKey.toString('base64url')
  ].join('$');
}

function hashPasswordSync(password: string) {
  const salt = randomBytes(16).toString('base64url');
  const derivedKey = scryptSync(password, salt, SCRYPT_KEY_LENGTH, SCRYPT_OPTIONS);
  return [
    'scrypt',
    String(SCRYPT_OPTIONS.N),
    String(SCRYPT_OPTIONS.r),
    String(SCRYPT_OPTIONS.p),
    salt,
    derivedKey.toString('base64url')
  ].join('$');
}

async function verifyPassword(password: string, storedHash: string) {
  const parts = storedHash.split('$');
  if (parts.length !== 6 || parts[0] !== 'scrypt') {
    return false;
  }

  const [, n, r, p, salt, encodedHash] = parts;
  const expected = Buffer.from(encodedHash, 'base64url');
  const actual = (await scrypt(password, salt, expected.length, {
    N: Number(n),
    r: Number(r),
    p: Number(p)
  })) as Buffer;

  return expected.length === actual.length && timingSafeEqual(expected, actual);
}

function hashSessionToken(token: string) {
  return createHash('sha256').update(token).digest('base64url');
}

function sessionExpiry() {
  return new Date(Date.now() + SESSION_DAYS * 24 * 60 * 60 * 1000).toISOString();
}

export function sessionCookieMaxAge() {
  return SESSION_DAYS * 24 * 60 * 60;
}

export function normalizeAuthUsername(username: unknown) {
  return normalizeUsername(username);
}

export function ensureDefaultAdminUser() {
  const timestamp = nowIso();
  runInTransaction((db) => {
    const userCount = db.prepare('SELECT COUNT(*) AS count FROM auth_users').get() as { count: number };
    if (Number(userCount.count) > 0) {
      return;
    }

    db.prepare(
      `
        INSERT INTO auth_users (
          id, username, username_normalized, password_hash, role, is_active,
          created_at, updated_at, last_login_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      `
    ).run(
      randomUUID(),
      DEFAULT_ADMIN_USERNAME,
      normalizeUsername(DEFAULT_ADMIN_USERNAME),
      hashPasswordSync(DEFAULT_ADMIN_PASSWORD),
      'admin',
      1,
      timestamp,
      timestamp,
      null
    );
  });
}

export async function createOrUpdateUser(input: {
  username: string;
  password: string;
  role: AuthRole;
  updateExisting?: boolean;
}) {
  const username = String(input.username ?? '').replace(/\s+/g, ' ').trim();
  const usernameNormalized = normalizeUsername(username);
  const role = validateRole(input.role);
  const password = String(input.password ?? '');
  if (!usernameNormalized) {
    throw new Error('Username is required.');
  }
  if (password.length < 8) {
    throw new Error('Password must be at least 8 characters.');
  }

  const passwordHash = await hashPassword(password);
  const timestamp = nowIso();
  let user: AuthUser | null = null;

  runInTransaction((db) => {
    const existing = db
      .prepare(
        `
          SELECT id, username, password_hash, role, is_active, created_at, updated_at, last_login_at
          FROM auth_users
          WHERE username_normalized = ?
          LIMIT 1
        `
      )
      .get(usernameNormalized) as AuthUserWithPasswordRow | undefined;

    if (existing && !input.updateExisting) {
      throw new Error('User already exists. Pass updateExisting to change it.');
    }

    const id = existing?.id ?? randomUUID();
    db.prepare(
      `
        INSERT INTO auth_users (
          id, username, username_normalized, password_hash, role, is_active,
          created_at, updated_at, last_login_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(username_normalized) DO UPDATE SET
          username = excluded.username,
          password_hash = excluded.password_hash,
          role = excluded.role,
          is_active = excluded.is_active,
          updated_at = excluded.updated_at
      `
    ).run(
      id,
      username,
      usernameNormalized,
      passwordHash,
      role,
      1,
      existing?.created_at ?? timestamp,
      timestamp,
      existing?.last_login_at ?? null
    );

    user = {
      id,
      username,
      role,
      isActive: true,
      createdAt: existing?.created_at ?? timestamp,
      updatedAt: timestamp,
      lastLoginAt: existing?.last_login_at ?? null
    };
  });

  return user;
}

export async function authenticateUser(username: unknown, password: unknown) {
  ensureDefaultAdminUser();
  const usernameNormalized = normalizeUsername(username);
  if (!usernameNormalized || typeof password !== 'string') {
    return null;
  }

  const row = getAppDatabase()
    .prepare(
      `
        SELECT id, username, password_hash, role, is_active, created_at, updated_at, last_login_at
        FROM auth_users
        WHERE username_normalized = ?
        LIMIT 1
      `
    )
    .get(usernameNormalized) as AuthUserWithPasswordRow | undefined;

  if (!row || !row.is_active || !(await verifyPassword(password, row.password_hash))) {
    return null;
  }

  const timestamp = nowIso();
  getAppDatabase().prepare('UPDATE auth_users SET last_login_at = ?, updated_at = ? WHERE id = ?').run(
    timestamp,
    timestamp,
    row.id
  );

  return {
    ...userFromRow(row),
    updatedAt: timestamp,
    lastLoginAt: timestamp
  };
}

export async function changeOwnPassword(input: {
  userId: string;
  currentPassword: string;
  nextPassword: string;
}) {
  const userId = String(input.userId ?? '');
  const currentPassword = String(input.currentPassword ?? '');
  const nextPassword = String(input.nextPassword ?? '');
  if (!userId) {
    throw new Error('Authentication required.');
  }
  if (nextPassword.length < 8) {
    throw new Error('Password must be at least 8 characters.');
  }

  const row = getAppDatabase()
    .prepare(
      `
        SELECT id, username, password_hash, role, is_active, created_at, updated_at, last_login_at
        FROM auth_users
        WHERE id = ?
        LIMIT 1
      `
    )
    .get(userId) as AuthUserWithPasswordRow | undefined;

  if (!row || !row.is_active) {
    throw new Error('Authentication required.');
  }
  if (!(await verifyPassword(currentPassword, row.password_hash))) {
    throw new Error('Current password is incorrect.');
  }

  const timestamp = nowIso();
  const passwordHash = await hashPassword(nextPassword);
  getAppDatabase()
    .prepare('UPDATE auth_users SET password_hash = ?, updated_at = ? WHERE id = ?')
    .run(passwordHash, timestamp, userId);

  return {
    ...userFromRow(row),
    updatedAt: timestamp
  };
}

export function createSession(userId: string) {
  const token = randomBytes(32).toString('base64url');
  const timestamp = nowIso();
  const expiresAt = sessionExpiry();
  getAppDatabase()
    .prepare(
      `
        INSERT INTO auth_sessions (id, user_id, token_hash, created_at, expires_at, last_seen_at)
        VALUES (?, ?, ?, ?, ?, ?)
      `
    )
    .run(randomUUID(), userId, hashSessionToken(token), timestamp, expiresAt, timestamp);
  return { token, expiresAt };
}

export function getUserForSessionToken(token: string | undefined | null) {
  if (!token) {
    return null;
  }

  const timestamp = nowIso();
  const row = getAppDatabase()
    .prepare(
      `
        SELECT
          u.id, u.username, u.role, u.is_active, u.created_at, u.updated_at, u.last_login_at
        FROM auth_sessions s
        JOIN auth_users u ON u.id = s.user_id
        WHERE s.token_hash = ? AND s.expires_at > ? AND u.is_active = 1
        LIMIT 1
      `
    )
    .get(hashSessionToken(token), timestamp) as AuthUserRow | undefined;

  if (!row) {
    return null;
  }

  getAppDatabase().prepare('UPDATE auth_sessions SET last_seen_at = ? WHERE token_hash = ?').run(
    timestamp,
    hashSessionToken(token)
  );
  return userFromRow(row);
}

export function deleteSessionToken(token: string | undefined | null) {
  if (!token) {
    return;
  }
  getAppDatabase().prepare('DELETE FROM auth_sessions WHERE token_hash = ?').run(hashSessionToken(token));
}

export function pruneExpiredSessions() {
  getAppDatabase().prepare('DELETE FROM auth_sessions WHERE expires_at <= ?').run(nowIso());
}

export function isAdmin(user: Pick<AuthUser, 'role'> | null | undefined) {
  return user?.role === 'admin';
}
