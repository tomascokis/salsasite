import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import {
  cleanupMediaTestEnvironment,
  derivedClip,
  moveAsset,
  setupMediaTestEnvironment,
  sourceAsset,
  writeLibrary
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-publish-audit');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('publishing clips records a non-undoable audit history action', async () => {
  const readyClip = derivedClip({
    status: 'ready',
    outputAssetId: 'output-asset',
    publishedAssetId: null,
    publishedAt: null
  });
  await writeLibrary(env.libraryPath, {
    videoAssets: [sourceAsset(), moveAsset({ id: 'output-asset' })],
    moveVideoLinks: [],
    derivedClips: [readyClip]
  });

  const { publishClipsToMoves } = await import('../src/lib/server/video-library.ts');
  const { listHistory, undoAction } = await import('../src/lib/server/history.ts');

  const published = await publishClipsToMoves([readyClip.id]);
  assert.equal(published.length, 1);
  assert.equal(published[0].publishedAssetId, 'output-asset');

  const publishEntry = listHistory(20).find((entry) => entry.type === 'media.clips.publish');
  assert.ok(publishEntry);
  assert.equal(publishEntry.entityType, 'media:clips');
  assert.equal(publishEntry.canUndo, false);
  assert.equal(publishEntry.undoUnavailableReason, 'This action type is not undoable yet');
  assert.equal(JSON.stringify(publishEntry).includes('"before"'), false);
  assert.equal(JSON.stringify(publishEntry).includes('"after"'), false);
  await assert.rejects(() => undoAction(publishEntry.id), /This action type is not undoable yet/);
});
