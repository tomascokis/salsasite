import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import {
  cleanupMediaTestEnvironment,
  readLibrary,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-service-boundaries');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('source and clip services own mutations while video-library remains a facade', async () => {
  const {
    createSourceAsset,
    deleteSourceAsset,
    updateSourceAsset
  } = await import('../src/lib/server/media-source-service.ts');
  const {
    saveSourceClips,
    setClipKeyVideo
  } = await import('../src/lib/server/media-clip-service.ts');
  const {
    getSourceAssets,
    setClipKeyVideo: facadeSetClipKeyVideo,
    updateSourceAsset: facadeUpdateSourceAsset
  } = await import('../src/lib/server/video-library.ts');
  const { listHistory, undoAction } = await import('../src/lib/server/history.ts');

  const created = await createSourceAsset({
    originalFilename: 'service-source.mp4',
    displayName: 'Service Source',
    dancers: ['A'],
    timing: 'on1',
    contentType: 'music',
    environment: 'class',
    originType: 'self-recorded',
    recordDate: null,
    classWorkshop: null,
    tags: [],
    notes: null,
    fileBuffer: Buffer.from('service-source-bytes')
  });
  assert.equal(created.reusedExisting, false);

  const duplicate = await createSourceAsset({
    originalFilename: 'service-source-copy.mp4',
    displayName: 'Service Source Copy',
    dancers: ['A'],
    timing: 'on1',
    contentType: 'music',
    environment: 'class',
    originType: 'self-recorded',
    recordDate: null,
    classWorkshop: null,
    tags: [],
    notes: null,
    fileBuffer: Buffer.from('service-source-bytes')
  });
  assert.equal(duplicate.reusedExisting, true);
  assert.equal(duplicate.asset.id, created.asset.id);

  await updateSourceAsset({
    assetId: created.asset.id,
    displayName: 'Service Source Updated',
    dancers: ['A', 'B'],
    timing: 'on2',
    contentType: 'counts',
    environment: 'social',
    originType: 'self-recorded',
    sourceUrl: null,
    createdAt: created.asset.createdAt,
    recordDate: '2026-06-02',
    classWorkshop: 'Workshop',
    tags: ['service'],
    notes: 'Updated by source service'
  });

  const clips = await saveSourceClips({
    sourceAssetId: created.asset.id,
    clips: [
      {
        moveId: 'RT000001',
        moveDisplayId: 'RT000001',
        isKeyVideo: true,
        label: null,
        descriptorLabel: null,
        startPositionId: null,
        endPositionId: null,
        timingGroupId: null,
        manuallyNamed: false,
        startMs: 0,
        endMs: 3000,
        actionStartMs: 500,
        actionEndMs: 2500,
        cropRect: null,
        countMarkers: [],
        countOverlayPlacement: 'top-left',
        countTimingPreset: 'on2-default'
      }
    ]
  });
  assert.equal(clips.length, 1);

  await setClipKeyVideo({ clipId: clips[0].id, isKeyVideo: false });
  await facadeSetClipKeyVideo({ clipId: clips[0].id, isKeyVideo: true });
  await facadeUpdateSourceAsset({
    assetId: created.asset.id,
    displayName: 'Facade Updated Source',
    dancers: ['A', 'B'],
    timing: 'on2',
    contentType: 'counts',
    environment: 'social',
    originType: 'self-recorded',
    sourceUrl: null,
    createdAt: created.asset.createdAt,
    recordDate: '2026-06-02',
    classWorkshop: 'Workshop',
    tags: ['service', 'facade'],
    notes: 'Updated through facade'
  });

  const facadeSources = await getSourceAssets();
  assert.equal(facadeSources.length, 1);
  assert.equal(facadeSources[0].displayName, 'Facade Updated Source');

  await deleteSourceAsset(created.asset.id);
  let library = await readLibrary(env.libraryPath);
  assert.equal(library.videoAssets.length, 0);
  assert.equal(library.derivedClips.length, 0);

  const deleteEntry = listHistory(20).find((entry) => entry.type === 'media.source.delete' && entry.status === 'active');
  assert.ok(deleteEntry);
  assert.equal(deleteEntry.canUndo, true);
  await undoAction(deleteEntry.id);

  library = await readLibrary(env.libraryPath);
  assert.equal(library.videoAssets.length, 1);
  assert.equal(library.derivedClips.length, 1);
  assert.equal(library.videoAssets[0].displayName, 'Facade Updated Source');
  assert.equal(library.derivedClips[0].isKeyVideo, true);

  const actionTypes = listHistory(50).map((entry) => entry.type);
  assert.ok(actionTypes.includes('media.source.create'));
  assert.ok(actionTypes.includes('media.source.update'));
  assert.ok(actionTypes.includes('media.clips.save'));
  assert.ok(actionTypes.includes('media.clip.key.update'));
  assert.ok(actionTypes.includes('media.source.delete'));
  assert.ok(actionTypes.includes('media.source.delete.undo'));
});
