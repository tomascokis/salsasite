import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const { tempRoot } = await setupMediaTestEnvironment('auth-server');

test.after(async () => {
  await cleanupMediaTestEnvironment(tempRoot);
});

test('default admin is created on first login attempt when no users exist', async () => {
  const { authenticateUser } = await import('../src/lib/server/auth.ts');

  const admin = await authenticateUser('tomascokis', 'superfollow');
  assert.equal(admin?.username, 'tomascokis');
  assert.equal(admin?.role, 'admin');
});

test('auth users can be created, authenticated, and loaded through sessions', async () => {
  const {
    authenticateUser,
    createOrUpdateUser,
    createSession,
    deleteSessionToken,
    getUserForSessionToken
  } = await import('../src/lib/server/auth.ts');

  const viewer = await createOrUpdateUser({
    username: 'Viewer',
    password: 'correct horse battery staple',
    role: 'viewer'
  });

  assert.equal(viewer?.username, 'Viewer');
  assert.equal(viewer?.role, 'viewer');
  await assert.rejects(
    () =>
      createOrUpdateUser({
        username: 'viewer',
        password: 'another password',
        role: 'admin'
      }),
    /User already exists/
  );

  assert.equal(await authenticateUser('viewer', 'wrong password'), null);
  const authed = await authenticateUser('viewer', 'correct horse battery staple');
  assert.equal(authed?.id, viewer?.id);
  assert.equal(authed?.lastLoginAt !== null, true);

  const session = createSession(viewer.id);
  const sessionUser = getUserForSessionToken(session.token);
  assert.equal(sessionUser?.id, viewer.id);
  deleteSessionToken(session.token);
  assert.equal(getUserForSessionToken(session.token), null);
});

test('route guards reject viewers and allow admins', async () => {
  const { requireAdmin } = await import('../src/lib/server/auth-guard.ts');
  const adminEvent = {
    locals: {
      user: {
        id: 'admin-user',
        username: 'admin',
        role: 'admin',
        isActive: true,
        createdAt: '2026-06-02T00:00:00.000Z',
        updatedAt: '2026-06-02T00:00:00.000Z',
        lastLoginAt: null
      }
    }
  };
  const viewerEvent = {
    locals: {
      user: {
        id: 'viewer-user',
        username: 'viewer',
        role: 'viewer',
        isActive: true,
        createdAt: '2026-06-02T00:00:00.000Z',
        updatedAt: '2026-06-02T00:00:00.000Z',
        lastLoginAt: null
      }
    }
  };

  assert.equal(requireAdmin(adminEvent).username, 'admin');
  assert.throws(() => requireAdmin(viewerEvent), (error) => error.status === 403);
});

test('action actor context records authenticated actor', async () => {
  const { listActions, recordAction, runInTransaction, runWithActionActor } = await import('../src/lib/server/app-state.ts');

  runWithActionActor('admin', () => {
    runInTransaction((db) => {
      recordAction(db, {
        type: 'auth.test',
        label: 'Auth actor test',
        entityType: 'test',
        entityId: 'actor',
        before: null,
        after: { ok: true }
      });
    });
  });

  const [latest] = listActions(1);
  assert.equal(latest.actor, 'admin');
});
