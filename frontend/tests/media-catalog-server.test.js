import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  derivedClip,
  exists,
  setupMediaTestEnvironment,
  sourceAsset
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-catalog');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

function source(id, displayName, overrides = {}) {
  return sourceAsset({
    id,
    filePath: `video-sources/${id}.mp4`,
    displayName,
    originalFilename: `${id}.mp4`,
    createdAt: `2026-06-02T00:00:0${id.slice(-1)}.000Z`,
    ...overrides
  });
}

await fs.writeFile(
  env.libraryPath,
  `${JSON.stringify({
    videoAssets: [
      { id: 'bad' },
      {
        ...source('source-1', 'External Source', {
          dancers: ['A', 'B'],
          tags: ['tag-a', 'tag-b']
        }),
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
          publishedAssetId: null,
          cropRect: { x: 0.1, y: 0.2, width: 0.7, height: 0.6 }
        }),
        countMarkers: [{ id: 'count-1', count: '1', ms: '1200', clear: true }]
      }
    ]
  })}\n`
);

test('media catalog repository bootstraps JSON into SQLite and treats SQLite as authoritative', async () => {
  const {
    defaultMediaCatalog,
    ensureMediaCatalogRoots,
    exportMediaCatalogSnapshot,
    mutateMediaCatalog,
    readMediaCatalog,
    sortMediaCatalog,
    writeMediaCatalog
  } = await import('../src/lib/server/media-catalog.ts');
  const { getAppDatabase } = await import('../src/lib/server/app-state.ts');

  assert.deepEqual(defaultMediaCatalog(), {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });

  await ensureMediaCatalogRoots();
  const normalized = await readMediaCatalog();
  assert.equal(normalized.videoAssets.length, 1);
  assert.equal(normalized.videoAssets[0].filePath, '/video-sources/source-1.mp4');
  assert.equal(normalized.videoAssets[0].timing, 'other');
  assert.equal(normalized.videoAssets[0].contentType, 'other');
  assert.equal(normalized.videoAssets[0].environment, 'class');
  assert.deepEqual(normalized.videoAssets[0].dancers, ['A', 'B']);
  assert.deepEqual(normalized.videoAssets[0].tags, ['tag-a', 'tag-b']);
  assert.deepEqual(normalized.moveVideoLinks, []);
  assert.equal(normalized.derivedClips.length, 1);
  assert.equal(normalized.derivedClips[0].moveId, 'RT000001');
  assert.equal(normalized.derivedClips[0].countMarkers[0].ms, 1200);
  assert.deepEqual(normalized.derivedClips[0].cropRect, { x: 0.1, y: 0.2, width: 0.7, height: 0.6 });
  assert.equal(await exists(path.join(env.dataDir, 'video-library.backup-before-sqlite.json')), false);

  const meta = getAppDatabase().prepare('SELECT value FROM app_state_meta WHERE key = ?').get('media_catalog_sqlite_v1');
  assert.equal(meta.value, 'complete');

  await fs.writeFile(
    env.libraryPath,
    `${JSON.stringify({ videoAssets: [source('source-2', 'Ignored JSON')], moveVideoLinks: [], derivedClips: [] }, null, 2)}\n`
  );
  assert.equal((await readMediaCatalog()).videoAssets[0].id, 'source-1');

  await writeMediaCatalog({
    videoAssets: [source('source-3', 'Written Source')],
    moveVideoLinks: [],
    derivedClips: []
  });
  const written = await readMediaCatalog();
  assert.equal(written.videoAssets[0].id, 'source-3');
  written.videoAssets[0].displayName = 'Mutated Clone';
  assert.equal((await readMediaCatalog()).videoAssets[0].displayName, 'Written Source');

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

  const delayedMutation = mutateMediaCatalog(async (library) => {
    await new Promise((resolve) => setTimeout(resolve, 25));
    library.videoAssets.push(source('source-6', 'Delayed Mutation'));
  });
  const readDuringMutation = readMediaCatalog();
  await delayedMutation;
  assert.equal((await readDuringMutation).videoAssets.some((asset) => asset.id === 'source-6'), true);

  await assert.rejects(
    () =>
      mutateMediaCatalog((library) => {
        library.videoAssets.push(source('source-7', 'Failed Mutation'));
        throw new Error('planned failure');
      }),
    /planned failure/
  );
  assert.equal((await readMediaCatalog()).videoAssets.some((asset) => asset.id === 'source-7'), false);

  const unsorted = await readMediaCatalog();
  sortMediaCatalog(unsorted);
  assert.deepEqual(unsorted.videoAssets.map((asset) => asset.id), ['source-3', 'source-4', 'source-5', 'source-6']);

  const exported = await exportMediaCatalogSnapshot(new Date('2026-06-03T04:05:06.789Z'));
  assert.equal(exported.filePath, 'media-catalog-exports/video-library-2026-06-03T04-05-06-789Z.json');
  assert.deepEqual(exported.counts, {
    videoAssets: 4,
    moveVideoLinks: 0,
    derivedClips: 0
  });
  const exportedCatalog = JSON.parse(await fs.readFile(exported.absolutePath, 'utf-8'));
  assert.deepEqual(
    exportedCatalog.videoAssets.map((asset) => asset.id),
    ['source-3', 'source-4', 'source-5', 'source-6']
  );
});
