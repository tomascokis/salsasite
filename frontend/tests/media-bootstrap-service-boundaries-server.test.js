import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment,
  writeFile
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-bootstrap-service-boundaries');

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

test('bootstrap service repairs legacy move assets while video-library facade stays compatible', async () => {
  await writeFile(path.join(env.mediaRoot, 'RT000001 Basic.mp4'), 'legacy move video');

  const { getVideoLibrary } = await import('../src/lib/server/media-bootstrap-service.ts');
  const { getVideoLibrary: facadeGetVideoLibrary } = await import('../src/lib/server/video-library.ts');

  const moves = [move()];
  const library = await getVideoLibrary(moves);
  const asset = library.videoAssets.find((entry) => entry.filePath === 'video-moves/RT000001 Basic.mp4');
  assert.ok(asset);
  assert.equal(asset.kind, 'move');
  assert.equal(library.moveVideoLinks.some((link) => link.moveId === 'RT000001' && link.assetId === asset.id), true);

  const facadeLibrary = await facadeGetVideoLibrary(moves);
  assert.deepEqual(facadeLibrary, library);
});
