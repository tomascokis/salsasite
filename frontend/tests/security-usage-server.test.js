import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const { tempRoot } = await setupMediaTestEnvironment('security-usage-server');

test.after(async () => {
  delete process.env.TRUST_PROXY_HEADERS;
  await cleanupMediaTestEnvironment(tempRoot);
});

test('authenticated usage is counted by hour and weekly unique account IP', async () => {
  const { createOrUpdateUser } = await import('../src/lib/server/auth.ts');
  const { getSecurityDashboard, recordAuthenticatedUsage } = await import(
    '../src/lib/server/security-usage.ts'
  );

  const user = await createOrUpdateUser({
    username: 'Usage Viewer',
    password: 'usage password',
    role: 'viewer'
  });
  const at = '2026-06-10T03:15:00.000Z';

  recordAuthenticatedUsage({ userId: user.id, kind: 'page', ipAddress: '198.51.100.10', at });
  recordAuthenticatedUsage({ userId: user.id, kind: 'page', ipAddress: '198.51.100.10', at });
  recordAuthenticatedUsage({ userId: user.id, kind: 'video', ipAddress: '198.51.100.10', at });
  recordAuthenticatedUsage({ userId: user.id, kind: 'video', ipAddress: '198.51.100.11', at });

  const dashboard = getSecurityDashboard({ now: '2026-06-10T03:30:00.000Z' });
  const usageUser = dashboard.users.find((entry) => entry.userId === user.id);

  assert.equal(usageUser?.pagesThisHour, 2);
  assert.equal(usageUser?.videosThisHour, 2);
  assert.equal(usageUser?.pagesLast24Hours, 2);
  assert.equal(usageUser?.videosLast24Hours, 2);
  assert.equal(usageUser?.uniqueIpsThisWeek, 2);
  assert.deepEqual(usageUser?.warningReasons, []);
});

test('security dashboard flags warning-only usage and login thresholds', async () => {
  const { createOrUpdateUser } = await import('../src/lib/server/auth.ts');
  const { getSecurityDashboard, recordAuthenticatedUsage, recordLoginAttempt } = await import(
    '../src/lib/server/security-usage.ts'
  );

  const user = await createOrUpdateUser({
    username: 'Warning Viewer',
    password: 'warning password',
    role: 'viewer'
  });
  const at = '2026-06-10T04:10:00.000Z';

  for (let index = 0; index < 121; index += 1) {
    recordAuthenticatedUsage({
      userId: user.id,
      kind: 'page',
      ipAddress: `203.0.113.${index % 4}`,
      at
    });
  }
  for (let index = 0; index < 41; index += 1) {
    recordAuthenticatedUsage({
      userId: user.id,
      kind: 'video',
      ipAddress: `203.0.113.${index % 4}`,
      at
    });
  }
  for (let index = 0; index < 11; index += 1) {
    recordLoginAttempt({
      username: 'Warning Viewer',
      ipAddress: '203.0.113.99',
      success: false,
      at
    });
  }

  const dashboard = getSecurityDashboard({ now: '2026-06-10T04:45:00.000Z' });
  const warningUser = dashboard.users.find((entry) => entry.userId === user.id);
  const failedIp = dashboard.failedAttemptsByIp.find((entry) => entry.ipAddress === '203.0.113.99');
  const failedUsername = dashboard.failedAttemptsByUsername.find((entry) => entry.username === 'warning viewer');

  assert.equal(warningUser?.pagesThisHour, 121);
  assert.equal(warningUser?.videosThisHour, 41);
  assert.equal(warningUser?.uniqueIpsThisWeek, 4);
  assert.equal(warningUser?.warningReasons.length, 3);
  assert.equal(failedIp?.attemptsThisHour, 11);
  assert.equal(failedIp?.warning, true);
  assert.equal(failedUsername?.attemptsThisHour, 11);
  assert.equal(failedUsername?.warning, true);
});

test('login views and attempts are visible in the security dashboard', async () => {
  const { createOrUpdateUser } = await import('../src/lib/server/auth.ts');
  const { getSecurityDashboard, recordLoginAttempt, recordLoginView } = await import(
    '../src/lib/server/security-usage.ts'
  );

  const user = await createOrUpdateUser({
    username: 'Login User',
    password: 'login password',
    role: 'viewer'
  });
  const at = '2026-06-10T05:20:00.000Z';

  recordLoginView({ ipAddress: '198.51.100.50', at });
  recordLoginView({ ipAddress: '198.51.100.50', at });
  recordLoginAttempt({
    username: 'Login User',
    userId: user.id,
    ipAddress: '198.51.100.50',
    success: true,
    at
  });
  recordLoginAttempt({
    username: 'Login User',
    ipAddress: '198.51.100.51',
    success: false,
    at
  });

  const dashboard = getSecurityDashboard({ now: '2026-06-10T05:55:00.000Z' });
  const views = dashboard.loginViewsByIp.find((entry) => entry.ipAddress === '198.51.100.50');
  const success = dashboard.successfulAttemptsByUsername.find((entry) => entry.username === 'login user');
  const recentSuccess = dashboard.recentAttempts.find(
    (entry) => entry.username === 'Login User' && entry.ipAddress === '198.51.100.50' && entry.success
  );

  assert.equal(views?.viewsThisHour, 2);
  assert.equal(views?.viewsLast24Hours, 2);
  assert.equal(success?.attemptsThisHour, 1);
  assert.equal(recentSuccess?.success, true);
});

test('client IP helper trusts proxy headers only when explicitly enabled', async () => {
  const { clientIpFromEvent } = await import('../src/lib/server/security-usage.ts');
  const event = {
    request: {
      headers: new Headers({
        'x-forwarded-for': '198.51.100.77, 10.0.0.10',
        'x-real-ip': '198.51.100.88'
      })
    },
    getClientAddress() {
      return '10.0.0.5';
    }
  };

  delete process.env.TRUST_PROXY_HEADERS;
  assert.equal(clientIpFromEvent(event), '10.0.0.5');

  process.env.TRUST_PROXY_HEADERS = 'true';
  assert.equal(clientIpFromEvent(event), '198.51.100.77');
});

test('failed login threshold creates a 24 hour IP ban', async () => {
  const {
    evaluateIpBanAfterLoginAttempt,
    getActiveIpBan,
    recordLoginAttempt
  } = await import('../src/lib/server/security-usage.ts');
  const ipAddress = '198.51.100.120';
  const at = '2026-06-10T07:00:00.000Z';

  for (let index = 0; index < 20; index += 1) {
    recordLoginAttempt({
      username: 'Threshold User',
      ipAddress,
      success: false,
      at
    });
  }

  const ban = evaluateIpBanAfterLoginAttempt(ipAddress, at);

  assert.equal(ban?.ipAddress, ipAddress);
  assert.equal(ban?.triggerKind, 'failed_login');
  assert.equal(ban?.triggerCount, 20);
  assert.equal(ban?.expiresAt, '2026-06-11T07:00:00.000Z');
  assert.equal(getActiveIpBan(ipAddress, '2026-06-10T08:00:00.000Z')?.id, ban?.id);
});

test('failed login threshold does not ban at 19 attempts or across different IPs', async () => {
  const {
    evaluateIpBanAfterLoginAttempt,
    recordLoginAttempt
  } = await import('../src/lib/server/security-usage.ts');
  const at = '2026-06-10T08:00:00.000Z';
  const almostIp = '198.51.100.121';
  const splitIpA = '198.51.100.122';
  const splitIpB = '198.51.100.123';

  for (let index = 0; index < 19; index += 1) {
    recordLoginAttempt({ username: 'Almost User', ipAddress: almostIp, success: false, at });
  }
  for (let index = 0; index < 10; index += 1) {
    recordLoginAttempt({ username: 'Split User', ipAddress: splitIpA, success: false, at });
    recordLoginAttempt({ username: 'Split User', ipAddress: splitIpB, success: false, at });
  }

  assert.equal(evaluateIpBanAfterLoginAttempt(almostIp, at), null);
  assert.equal(evaluateIpBanAfterLoginAttempt(splitIpA, at), null);
  assert.equal(evaluateIpBanAfterLoginAttempt(splitIpB, at), null);
});

test('login view threshold creates a 24 hour IP ban', async () => {
  const {
    evaluateIpBanAfterLoginView,
    getActiveIpBan,
    recordLoginView
  } = await import('../src/lib/server/security-usage.ts');
  const ipAddress = '198.51.100.124';
  const at = '2026-06-10T09:30:00.000Z';

  for (let index = 0; index < 120; index += 1) {
    recordLoginView({ ipAddress, at });
  }

  const ban = evaluateIpBanAfterLoginView(ipAddress, at);

  assert.equal(ban?.ipAddress, ipAddress);
  assert.equal(ban?.triggerKind, 'login_view');
  assert.equal(ban?.triggerCount, 120);
  assert.equal(ban?.windowStartedAt, '2026-06-10T09:00:00.000Z');
  assert.equal(getActiveIpBan(ipAddress, '2026-06-10T10:00:00.000Z')?.id, ban?.id);
});

test('expired bans stop applying and admin unban lifts active bans', async () => {
  const {
    evaluateIpBanAfterLoginAttempt,
    getActiveIpBan,
    recordLoginAttempt,
    unbanIpAddress
  } = await import('../src/lib/server/security-usage.ts');
  const expiredIp = '198.51.100.125';
  const unbanIp = '198.51.100.126';
  const at = '2026-06-10T10:00:00.000Z';

  for (let index = 0; index < 20; index += 1) {
    recordLoginAttempt({ username: 'Expired User', ipAddress: expiredIp, success: false, at });
    recordLoginAttempt({ username: 'Unban User', ipAddress: unbanIp, success: false, at });
  }

  evaluateIpBanAfterLoginAttempt(expiredIp, at);
  evaluateIpBanAfterLoginAttempt(unbanIp, at);

  assert.equal(getActiveIpBan(expiredIp, '2026-06-11T09:59:59.000Z')?.active, true);
  assert.equal(getActiveIpBan(expiredIp, '2026-06-11T10:00:01.000Z'), null);
  assert.equal(unbanIpAddress(unbanIp, 'admin', 'test unban'), 1);
  assert.equal(getActiveIpBan(unbanIp, '2026-06-10T11:00:00.000Z'), null);
});

test('active ban blocks login action before password verification', async () => {
  const { createOrUpdateUser } = await import('../src/lib/server/auth.ts');
  const { getAppDatabase } = await import('../src/lib/server/app-state.ts');
  const {
    evaluateIpBanAfterLoginAttempt,
    recordLoginAttempt
  } = await import('../src/lib/server/security-usage.ts');
  const { actions } = await import('../src/routes/login/+page.server.ts');
  const ipAddress = '198.51.100.127';
  const at = '2026-06-10T11:00:00.000Z';

  await createOrUpdateUser({
    username: 'Banned Login',
    password: 'correct password',
    role: 'viewer'
  });
  for (let index = 0; index < 20; index += 1) {
    recordLoginAttempt({ username: 'Banned Login', ipAddress, success: false, at });
  }
  evaluateIpBanAfterLoginAttempt(ipAddress, at);

  const beforeAttempts = getAppDatabase()
    .prepare('SELECT COUNT(*) AS count FROM security_login_attempts WHERE ip_address = ?')
    .get(ipAddress).count;
  const result = await actions.default({
    request: {
      headers: new Headers(),
      async formData() {
        const formData = new FormData();
        formData.set('username', 'Banned Login');
        formData.set('password', 'correct password');
        return formData;
      }
    },
    cookies: {
      set() {
        throw new Error('Session should not be created for banned IP.');
      }
    },
    url: new URL('http://example.test/login'),
    getClientAddress() {
      return ipAddress;
    }
  });
  const afterAttempts = getAppDatabase()
    .prepare('SELECT COUNT(*) AS count FROM security_login_attempts WHERE ip_address = ?')
    .get(ipAddress).count;

  assert.equal(result.status, 429);
  assert.equal(result.data.banned, true);
  assert.equal(afterAttempts, beforeAttempts);
});
