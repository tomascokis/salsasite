import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const { tempRoot } = await setupMediaTestEnvironment('history-server');

test.after(async () => {
  await cleanupMediaTestEnvironment(tempRoot);
});

test('history API returns sanitized undo metadata and blocks unsupported undo paths', async () => {
  const { recordAction, runInTransaction } = await import('../src/lib/server/app-state.ts');
  const { listHistory, undoAction } = await import('../src/lib/server/history.ts');
  const { GET } = await import('../src/routes/api/history/+server.ts');

  let metadataActionId = '';
  let unsupportedActionId = '';
  let invalidSourceDeleteActionId = '';
  runInTransaction((db) => {
    metadataActionId = recordAction(db, {
      type: 'metadata.topic.update',
      label: 'Updated topic Test',
      entityType: 'metadata:topic',
      entityId: 'topic:test',
      before: null,
      after: {
        id: 'topic:test',
        slug: 'test',
        name: 'Test',
        description: null,
        createdAt: '2026-06-02T00:00:00.000Z',
        updatedAt: '2026-06-02T00:00:00.000Z',
        source: 'custom'
      }
    }).id;
    unsupportedActionId = recordAction(db, {
      type: 'clip.render',
      label: 'Rendered clip',
      entityType: 'media:clip',
      entityId: 'clip-1',
      before: {},
      after: {}
    }).id;
    invalidSourceDeleteActionId = recordAction(db, {
      type: 'media.source.delete',
      label: 'Deleted source video: Invalid',
      entityType: 'media:source',
      entityId: 'source-invalid',
      before: {},
      after: {}
    }).id;
  });

  const response = await GET({
    url: new URL('http://test.local/api/history?limit=not-a-number'),
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
  });
  const payload = await response.json();
  assert.equal(response.status, 200);
  assert.equal(Array.isArray(payload.entries), true);
  assert.equal(payload.entries.length, 3);
  assert.equal(JSON.stringify(payload.entries).includes('before'), false);
  assert.equal(JSON.stringify(payload.entries).includes('after'), false);

  const unsupportedEntry = payload.entries.find((entry) => entry.id === unsupportedActionId);
  assert.ok(unsupportedEntry);
  assert.equal(unsupportedEntry.canUndo, false);
  assert.equal(unsupportedEntry.undoUnavailableReason, 'This action type is not undoable yet');

  await assert.rejects(() => undoAction(unsupportedActionId), /This action type is not undoable yet/);
  await assert.rejects(() => undoAction(invalidSourceDeleteActionId), /Invalid media delete snapshot/);

  await undoAction(metadataActionId);
  await assert.rejects(() => undoAction(metadataActionId), /Already undone/);

  const entriesAfterUndo = listHistory(10);
  const undoneEntry = entriesAfterUndo.find((entry) => entry.id === metadataActionId);
  assert.ok(undoneEntry);
  assert.equal(undoneEntry.status, 'undone');
  assert.equal(undoneEntry.canUndo, false);
  assert.equal(undoneEntry.undoUnavailableReason, 'Already undone');
  assert.ok(undoneEntry.details.undoneByActionId);

  const undoEntry = entriesAfterUndo.find((entry) => entry.undoOfActionId === metadataActionId);
  assert.ok(undoEntry);
  assert.equal(undoEntry.canUndo, false);
  assert.equal(undoEntry.undoUnavailableReason, 'Undo actions cannot be undone');
  await assert.rejects(() => undoAction(undoEntry.id), /Undo actions cannot be undone/);
});
