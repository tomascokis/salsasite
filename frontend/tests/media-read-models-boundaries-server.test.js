import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  derivedClip,
  moveAsset,
  setupMediaTestEnvironment,
  sourceAsset,
  writeFile,
  writeLibrary
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-read-models-boundaries');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

function move(overrides = {}) {
  return {
    id: 'RT000001',
    displayId: null,
    slug: 'rt000001',
    name: 'Basic',
    topic: null,
    level: null,
    type: null,
    category: null,
    group: null,
    baseMove: null,
    components: null,
    parentIds: [],
    childIds: [],
    relatedMoveIds: [],
    positions: null,
    seeAlso: null,
    tags: null,
    description: null,
    source: null,
    comments: null,
    moveOrder: null,
    topicCol: null,
    topicOrder: null,
    familyOrder: null,
    valid: true,
    errors: null,
    hasLocalVideo: false,
    videoFiles: [],
    videoLinks: [],
    previewVideoFile: null,
    ...overrides
  };
}

test('read-model service returns the same data as the video-library facade', async () => {
  await writeLibrary(env.libraryPath, {
    videoAssets: [sourceAsset(), moveAsset()],
    moveVideoLinks: [
      { id: 'link-1', moveId: 'RT000001', assetId: 'output-asset', order: 0, createdAt: '2026-06-02T00:00:02.000Z' }
    ],
    derivedClips: [derivedClip()]
  });
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4'), 'full');
  await writeFile(path.join(env.posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12.jpg'), 'poster');

  const readModels = await import('../src/lib/server/media-read-models.ts');
  const facade = await import('../src/lib/server/video-library.ts');
  const moves = [move()];

  assert.deepEqual(
    await readModels.getResolvedMoveVideos('RT000001', moves),
    await facade.getResolvedMoveVideos('RT000001', moves)
  );
  assert.deepEqual(
    [...(await readModels.buildResolvedMoveVideoIndex(moves)).entries()],
    [...(await facade.buildResolvedMoveVideoIndex(moves)).entries()]
  );
  assert.deepEqual(
    [...(await readModels.buildResolvedMoveVideoMetadataIndex(moves)).entries()],
    [...(await facade.buildResolvedMoveVideoMetadataIndex(moves)).entries()]
  );
  assert.deepEqual(await readModels.getSourceAssets(moves), await facade.getSourceAssets(moves));
  assert.deepEqual(await readModels.getUploadPageData(moves), await facade.getUploadPageData(moves));
  assert.deepEqual(await readModels.getMediaLibraryPage(moves, { limit: 10 }), await facade.getMediaLibraryPage(moves, { limit: 10 }));
  assert.deepEqual(await readModels.getRenderStatuses(['abcdef12-0000-4000-9000-000000000000']), await facade.getRenderStatuses(['abcdef12-0000-4000-9000-000000000000']));
  assert.deepEqual(await readModels.getVideoLibrarySummary(moves), await facade.getVideoLibrarySummary(moves));
});
