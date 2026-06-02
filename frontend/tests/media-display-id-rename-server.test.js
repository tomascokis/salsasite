import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
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

const env = await setupMediaTestEnvironment('display-id-rename');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('syncDerivedClipDisplayIdForMove renames generated files and posters through media manager', async () => {
  await writeLibrary(env.libraryPath, {
    videoAssets: [
      sourceAsset(),
      moveAsset()
    ],
    moveVideoLinks: [
      { id: 'link-1', moveId: 'RT000001', assetId: 'output-asset', order: 0, createdAt: '2026-06-02T00:00:02.000Z' }
    ],
    derivedClips: [derivedClip()]
  });

  const oldFiles = [
    'RT000001 Source Clip abcdef12.mp4',
    'RT000001 Source Clip abcdef12 action.mp4',
    'RT000001 Source Clip abcdef12 low.mp4',
    'RT000001 Source Clip abcdef12 padded low.mp4'
  ];
  for (const file of oldFiles) {
    await writeFile(path.join(env.mediaRoot, file), file);
  }
  await writeFile(path.join(env.posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12.jpg'), 'poster');

  const { syncDerivedClipDisplayIdForMove } = await import('../src/lib/server/video-library.ts');
  const { listMediaJobsWithFileActions } = await import('../src/lib/server/media-manager.ts');

  await syncDerivedClipDisplayIdForMove('RT000001', 'RT999999');

  const library = await readLibrary(env.libraryPath);
  const clip = library.derivedClips[0];
  const outputAsset = library.videoAssets.find((asset) => asset.id === 'output-asset');
  assert.equal(clip.moveDisplayId, 'RT999999');
  assert.equal(outputAsset.filePath, 'video-moves/RT999999 Source Clip abcdef12.mp4');
  assert.equal(clip.actionOutputFilePath, 'video-moves/RT999999 Source Clip abcdef12 action.mp4');
  assert.equal(clip.lowResOutputFilePath, 'video-moves/RT999999 Source Clip abcdef12 low.mp4');
  assert.equal(clip.lowResPaddedOutputFilePath, 'video-moves/RT999999 Source Clip abcdef12 padded low.mp4');

  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4')), false);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT999999 Source Clip abcdef12.mp4')), true);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT999999 Source Clip abcdef12 action.mp4')), true);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT999999 Source Clip abcdef12 low.mp4')), true);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT999999 Source Clip abcdef12 padded low.mp4')), true);
  assert.equal(await exists(path.join(env.posterRoot, 'video-moves', 'RT999999 Source Clip abcdef12.jpg')), true);

  const renameJob = listMediaJobsWithFileActions(20).find((job) =>
    job.fileActions.some((action) => action.actionType === 'rename-video')
  );
  assert.ok(renameJob);
  assert.equal(renameJob.fileActions.some((action) => action.actionType === 'rename-poster'), true);
  assert.equal(JSON.stringify(renameJob.fileActions).includes('originalAbsolutePath'), false);
});
