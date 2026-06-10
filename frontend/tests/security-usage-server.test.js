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
