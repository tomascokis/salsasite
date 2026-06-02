import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment,
  writeFile,
  writeLibrary
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-read-repair-boundaries');

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

test('read-model paths do not write legacy repair changes to the media catalog', async () => {
  await writeLibrary(env.libraryPath, {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });
  await writeFile(path.join(env.mediaRoot, 'RT000001 Basic.mp4'), 'legacy move video');

  const before = await fs.readFile(env.libraryPath, 'utf-8');
  const readModels = await import('../src/lib/server/media-read-models.ts');
  const moves = [move()];

  assert.deepEqual(await readModels.getResolvedMoveVideos('RT000001', moves), []);
  assert.deepEqual(await readModels.getUploadPageData(moves), {
    assets: [],
    library: {
      videoAssets: [],
      moveVideoLinks: [],
      derivedClips: []
    }
  });

  const after = await fs.readFile(env.libraryPath, 'utf-8');
  assert.equal(after, before);
});

test('explicit bootstrap repair path still persists legacy move assets', async () => {
  const { getVideoLibraryWithRepairs, getVideoLibrary } = await import('../src/lib/server/media-bootstrap-service.ts');
  const moves = [move()];

  const repaired = await getVideoLibraryWithRepairs(moves);
  const asset = repaired.videoAssets.find((entry) => entry.filePath === 'video-moves/RT000001 Basic.mp4');

  assert.ok(asset);
  assert.equal(asset.kind, 'move');
  assert.equal(repaired.moveVideoLinks.some((link) => link.moveId === 'RT000001' && link.assetId === asset.id), true);

  const persisted = JSON.parse(await fs.readFile(env.libraryPath, 'utf-8'));
  assert.equal(persisted.videoAssets.some((entry) => entry.id === asset.id), true);
  assert.deepEqual(await getVideoLibrary(moves), repaired);
});
