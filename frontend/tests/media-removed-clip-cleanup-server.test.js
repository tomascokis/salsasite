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

const env = await setupMediaTestEnvironment('removed-clip-cleanup');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('saveSourceClips trashes generated files for removed clips', async () => {
  const retainedClip = derivedClip({
    id: '11111111-0000-4000-9000-000000000000',
    outputAssetId: 'retained-asset',
    publishedAssetId: 'retained-asset'
  });
  const removedClip = derivedClip({
    id: '22222222-0000-4000-9000-000000000000',
    outputAssetId: 'removed-asset',
    publishedAssetId: 'removed-asset',
    actionOutputFilePath: 'video-moves/RT000001 Removed Clip 22222222 action.mp4',
    lowResOutputFilePath: 'video-moves/RT000001 Removed Clip 22222222 low.mp4',
    lowResPaddedOutputFilePath: 'video-moves/RT000001 Removed Clip 22222222 padded low.mp4',
    publishedActionOutputFilePath: 'video-moves/RT000001 Removed Clip 22222222 action.mp4',
    publishedLowResFilePath: 'video-moves/RT000001 Removed Clip 22222222 low.mp4',
    publishedLowResPaddedFilePath: 'video-moves/RT000001 Removed Clip 22222222 padded low.mp4'
  });
  await writeLibrary(env.libraryPath, {
    videoAssets: [
      sourceAsset(),
      moveAsset({ id: 'retained-asset', filePath: 'video-moves/RT000001 Source Clip abcdef12.mp4' }),
      moveAsset({
        id: 'removed-asset',
        filePath: 'video-moves/RT000001 Removed Clip 22222222.mp4',
        originalFilename: 'RT000001 Removed Clip 22222222.mp4'
      })
    ],
    moveVideoLinks: [
      { id: 'retained-link', moveId: 'RT000001', assetId: 'retained-asset', order: 0, createdAt: '2026-06-02T00:00:02.000Z' },
      { id: 'removed-link', moveId: 'RT000001', assetId: 'removed-asset', order: 1, createdAt: '2026-06-02T00:00:02.000Z' }
    ],
    derivedClips: [retainedClip, removedClip]
  });

  const removedFiles = [
    'RT000001 Removed Clip 22222222.mp4',
    'RT000001 Removed Clip 22222222 action.mp4',
    'RT000001 Removed Clip 22222222 low.mp4',
    'RT000001 Removed Clip 22222222 padded low.mp4'
  ];
  const retainedFile = path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4');
  await writeFile(path.join(env.sourceRoot, 'source.mp4'), 'source');
  await writeFile(retainedFile, 'retained');
  await writeFile(path.join(env.posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12.jpg'), 'retained-poster');
  for (const file of removedFiles) {
    await writeFile(path.join(env.mediaRoot, file), file);
  }
  await writeFile(path.join(env.posterRoot, 'video-moves', 'RT000001 Removed Clip 22222222.jpg'), 'removed-poster');

  const { saveSourceClips } = await import('../src/lib/server/video-library.ts');
  const { listMediaJobsWithFileActions } = await import('../src/lib/server/media-manager.ts');
  const { listHistory } = await import('../src/lib/server/history.ts');

  await saveSourceClips({
    sourceAssetId: 'source-asset',
    clips: [
      {
        id: retainedClip.id,
        moveId: retainedClip.moveId,
        moveDisplayId: retainedClip.moveDisplayId,
        isKeyVideo: retainedClip.isKeyVideo,
        label: retainedClip.label,
        descriptorLabel: retainedClip.descriptorLabel,
        startPositionId: retainedClip.startPositionId,
        endPositionId: retainedClip.endPositionId,
        timingGroupId: retainedClip.timingGroupId,
        manuallyNamed: retainedClip.manuallyNamed,
        startMs: retainedClip.startMs,
        endMs: retainedClip.endMs,
        actionStartMs: retainedClip.actionStartMs,
        actionEndMs: retainedClip.actionEndMs,
        cropRect: retainedClip.cropRect,
        countMarkers: retainedClip.countMarkers,
        countOverlayPlacement: retainedClip.countOverlayPlacement,
        countTimingPreset: retainedClip.countTimingPreset
      }
    ]
  });

  const library = await readLibrary(env.libraryPath);
  assert.deepEqual(library.derivedClips.map((clip) => clip.id), [retainedClip.id]);
  assert.equal(library.videoAssets.some((asset) => asset.id === 'removed-asset'), false);
  assert.equal(await exists(retainedFile), true);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Removed Clip 22222222.mp4')), false);

  const cleanupJob = listMediaJobsWithFileActions(20).find((job) =>
    job.fileActions.some((action) =>
      action.actionType === 'cleanup-generated' &&
      action.metadata &&
      typeof action.metadata === 'object' &&
      action.metadata.cleanupReason === 'removed-source-clips'
    )
  );
  assert.ok(cleanupJob);
  assert.equal(await exists(path.join(env.dataDir, 'media-trash', cleanupJob.id, 'video-moves', 'RT000001 Removed Clip 22222222.mp4')), true);
  assert.equal(
    await exists(path.join(env.dataDir, 'media-trash', cleanupJob.id, 'video-posters', 'video-moves', 'RT000001 Removed Clip 22222222.jpg')),
    true
  );

  const saveAction = listHistory(20).find((entry) => entry.type === 'media.clips.save');
  assert.ok(saveAction);
  assert.equal(saveAction.canUndo, false);
  assert.equal(saveAction.undoUnavailableReason, 'This action type is not undoable yet');
});
