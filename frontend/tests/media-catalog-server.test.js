import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';

import {
  cleanupMediaTestEnvironment,
  derivedClip,
  readLibrary,
  setupMediaTestEnvironment,
  sourceAsset
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-catalog');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

function source(id, displayName) {
  return sourceAsset({
    id,
    filePath: `video-sources/${id}.mp4`,
    displayName,
    originalFilename: `${id}.mp4`,
    createdAt: `2026-06-02T00:00:0${id.slice(-1)}.000Z`
  });
}

test('media catalog repository reads defaults, normalizes JSON, writes cache, and serializes mutations', async () => {
  const {
    defaultMediaCatalog,
    ensureMediaCatalogRoots,
    mutateMediaCatalog,
    readMediaCatalog,
    sortMediaCatalog,
    writeMediaCatalog
  } = await import('../src/lib/server/media-catalog.ts');

  assert.deepEqual(defaultMediaCatalog(), {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });
  assert.deepEqual(await readMediaCatalog(), {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });

  await ensureMediaCatalogRoots();
  await fs.writeFile(
    env.libraryPath,
    `${JSON.stringify({
      videoAssets: [
        { id: 'bad' },
        {
          ...source('source-1', 'External Source'),
          timing: 'bad',
          contentType: 'bad',
          environment: 'bad',
          filePath: '/video-sources/source-1.mp4'
        }
      ],
      moveVideoLinks: 'not-an-array',
      derivedClips: [
        { id: 'bad-clip' },
        {
          ...derivedClip({
            id: 'clip-1',
            sourceAssetId: 'source-1',
            moveId: 'rt000001',
            outputAssetId: null,
            publishedAssetId: null
          }),
          countMarkers: [{ id: 'count-1', count: '1', ms: '1200', clear: true }]
        }
      ]
    })}\n`
  );

  const normalized = await readMediaCatalog();
  assert.equal(normalized.videoAssets.length, 1);
  assert.equal(normalized.videoAssets[0].filePath, '/video-sources/source-1.mp4');
  assert.equal(normalized.videoAssets[0].timing, 'other');
  assert.equal(normalized.videoAssets[0].contentType, 'other');
  assert.equal(normalized.videoAssets[0].environment, 'class');
  assert.deepEqual(normalized.moveVideoLinks, []);
  assert.equal(normalized.derivedClips.length, 1);
  assert.equal(normalized.derivedClips[0].moveId, 'RT000001');
  assert.equal(normalized.derivedClips[0].countMarkers[0].ms, 1200);

  const catalog = {
    videoAssets: [source('source-2', 'Written Source')],
    moveVideoLinks: [],
    derivedClips: []
  };
  await writeMediaCatalog(catalog);
  const written = await readLibrary(env.libraryPath);
  assert.equal(written.videoAssets[0].id, 'source-2');
  assert.equal((await fs.readFile(env.libraryPath, 'utf-8')).endsWith('\n'), true);
  const cached = await readMediaCatalog();
  assert.equal(cached.videoAssets[0].id, 'source-2');
  cached.videoAssets[0].displayName = 'Mutated Clone';
  assert.equal((await readMediaCatalog()).videoAssets[0].displayName, 'Written Source');

  await new Promise((resolve) => setTimeout(resolve, 5));
  await fs.writeFile(
    env.libraryPath,
    `${JSON.stringify({ videoAssets: [source('source-3', 'Externally Written')], moveVideoLinks: [], derivedClips: [] }, null, 2)}\n`
  );
  assert.equal((await readMediaCatalog()).videoAssets[0].id, 'source-3');

  await Promise.all([
    mutateMediaCatalog((library) => {
      library.videoAssets.push(source('source-4', 'Concurrent A'));
    }),
    mutateMediaCatalog((library) => {
      library.videoAssets.push(source('source-5', 'Concurrent B'));
    })
  ]);
  assert.deepEqual(
    (await readMediaCatalog()).videoAssets.map((asset) => asset.id).sort(),
    ['source-3', 'source-4', 'source-5']
  );

  await assert.rejects(
    () =>
      mutateMediaCatalog((library) => {
        library.videoAssets.push(source('source-6', 'Failed Mutation'));
        throw new Error('planned failure');
      }),
    /planned failure/
  );
  assert.equal((await readMediaCatalog()).videoAssets.some((asset) => asset.id === 'source-6'), false);

  const unsorted = await readMediaCatalog();
  sortMediaCatalog(unsorted);
  assert.deepEqual(unsorted.videoAssets.map((asset) => asset.id), ['source-3', 'source-4', 'source-5']);
});
