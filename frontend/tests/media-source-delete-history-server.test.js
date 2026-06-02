import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  derivedClip,
  exists,
  moveAsset,
  readLibrary,
  setupMediaTestEnvironment,
  sourceAsset,
  writeFile,
  writeLibrary
} from './media-server-fixtures.js';

const { tempRoot, dataDir, mediaRoot, sourceRoot, posterRoot, libraryPath } =
  await setupMediaTestEnvironment('source-delete-history');

test.after(async () => {
  await cleanupMediaTestEnvironment(tempRoot);
});

async function seedSourceLibrary() {
  const source = sourceAsset();
  const output = moveAsset();
  const clip = derivedClip();

  await writeFile(path.join(sourceRoot, 'source.mp4'), 'source');
  await writeFile(path.join(mediaRoot, 'RT000001 Source Clip abcdef12.mp4'), 'full');
  await writeFile(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 action.mp4'), 'action');
  await writeFile(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4'), 'low');
  await writeFile(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 padded low.mp4'), 'padded-low');
  await writeFile(path.join(posterRoot, 'video-sources', 'source.jpg'), 'source-poster');
  await writeFile(path.join(posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12.jpg'), 'full-poster');
  await writeFile(path.join(posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12 action.jpg'), 'action-poster');
  await writeFile(path.join(posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12 low.jpg'), 'low-poster');
  await writeFile(path.join(posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12 padded low.jpg'), 'padded-low-poster');

  await writeLibrary(libraryPath, {
    videoAssets: [source, output],
    moveVideoLinks: [
      {
        id: 'link-1',
        moveId: 'RT000001',
        assetId: output.id,
        order: 0,
        createdAt: '2026-06-02T00:00:02.000Z'
      }
    ],
    derivedClips: [clip]
  });
}

test('source delete history restores variants and missing trash fails without catalog restore', async () => {
  const { deleteSourceAsset } = await import('../src/lib/server/video-library.ts');
  const { listHistory, undoAction } = await import('../src/lib/server/history.ts');

  await seedSourceLibrary();
  const firstDelete = await deleteSourceAsset('source-asset');
  assert.deepEqual(firstDelete.deletedAssetIds.sort(), ['output-asset', 'source-asset']);
  assert.deepEqual(firstDelete.deletedClipIds, ['abcdef12-0000-4000-9000-000000000000']);

  let deletedLibrary = await readLibrary(libraryPath);
  assert.equal(deletedLibrary.videoAssets.length, 0);
  assert.equal(deletedLibrary.derivedClips.length, 0);
  assert.equal(deletedLibrary.moveVideoLinks.length, 0);
  assert.equal(await exists(path.join(sourceRoot, 'source.mp4')), false);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 padded low.mp4')), false);

  const firstDeleteAction = listHistory(20).find((entry) => entry.type === 'media.source.delete' && entry.status === 'active');
  assert.ok(firstDeleteAction);
  assert.equal(firstDeleteAction.canUndo, true);
  assert.ok(firstDeleteAction.details.mediaJobId);

  await undoAction(firstDeleteAction.id);
  const restoredLibrary = await readLibrary(libraryPath);
  assert.equal(restoredLibrary.videoAssets.length, 2);
  assert.equal(restoredLibrary.derivedClips.length, 1);
  assert.equal(restoredLibrary.moveVideoLinks.length, 1);
  assert.equal(await exists(path.join(sourceRoot, 'source.mp4')), true);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12.mp4')), true);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 action.mp4')), true);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4')), true);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 padded low.mp4')), true);
  assert.equal(await exists(path.join(posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12 padded low.jpg')), true);

  await deleteSourceAsset('source-asset');
  const secondDeleteAction = listHistory(20).find((entry) => entry.type === 'media.source.delete' && entry.status === 'active');
  assert.ok(secondDeleteAction);
  const missingTrashPath = path.join(dataDir, 'media-trash', secondDeleteAction.details.mediaJobId, 'video-sources', 'source.mp4');
  await fs.rm(missingTrashPath, { force: true });

  await assert.rejects(() => undoAction(secondDeleteAction.id), /Missing trashed media file/);
  deletedLibrary = await readLibrary(libraryPath);
  assert.equal(deletedLibrary.videoAssets.length, 0);
  assert.equal(deletedLibrary.derivedClips.length, 0);
  assert.equal(deletedLibrary.moveVideoLinks.length, 0);
  assert.equal(await exists(path.join(sourceRoot, 'source.mp4')), false);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12.mp4')), false);
  assert.equal(await exists(path.join(mediaRoot, 'RT000001 Source Clip abcdef12 action.mp4')), false);
});
