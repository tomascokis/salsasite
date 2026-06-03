import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'salsa-catalog-data-'));
const dataDir = path.join(tempRoot, 'data');
const catalogBootstrapDir = path.join(dataDir, 'bootstrap', 'catalog');
const mediaRoot = path.join(tempRoot, 'media', 'video-moves');
const sourceRoot = path.join(tempRoot, 'media', 'video-sources');
const posterRoot = path.join(tempRoot, 'media', 'video-posters');

process.env.DATA_DIR = dataDir;
process.env.MEDIA_ROOT = mediaRoot;
process.env.SOURCE_ROOT = sourceRoot;
process.env.POSTER_ROOT = posterRoot;

await Promise.all([
  fs.mkdir(catalogBootstrapDir, { recursive: true }),
  fs.mkdir(mediaRoot, { recursive: true }),
  fs.mkdir(sourceRoot, { recursive: true }),
  fs.mkdir(posterRoot, { recursive: true })
]);

test.after(async () => {
  await fs.rm(tempRoot, { recursive: true, force: true });
});

const move = {
  id: 'RT000001',
  displayId: null,
  slug: 'right-turn',
  name: 'Right turn',
  topic: 'Turns',
  level: '1',
  type: null,
  category: null,
  group: 'Turns',
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
  moveOrder: 1,
  topicCol: 1,
  topicOrder: 1,
  familyOrder: 1,
  valid: true,
  errors: null,
  hasLocalVideo: false,
  videoFiles: [],
  videoLinks: [],
  previewVideoFile: null
};

await fs.writeFile(
  path.join(catalogBootstrapDir, 'manifest.json'),
  `${JSON.stringify({
    generatedAt: '2026-06-03T00:00:00.000Z',
    source: {
      rawMoveReference: 'data/legacy/reference/data_reference.xlsx',
      moves: 'data/live/bootstrap/catalog/moves.json',
      layout: 'data/live/bootstrap/catalog/layout.json',
      progress: 'data/live/bootstrap/catalog/progress.json',
      localVideoDirectory: null,
      visualReference: ''
    },
    counts: {
      moveRows: 1,
      validMoves: 1,
      layoutRows: 1,
      layoutColumns: 1,
      progressRows: 1,
      progressSnapshots: 1,
      trackableMoves: 1,
      movesWithLocalVideo: 0
    },
    routes: {
      home: '/',
      moveDetail: '/moves/[slug]',
      progress: '/progress',
      progressEditor: '/progress/editor'
    }
  })}\n`
);
await fs.writeFile(path.join(catalogBootstrapDir, 'moves.json'), `${JSON.stringify([move])}\n`);
await fs.writeFile(
  path.join(catalogBootstrapDir, 'layout.json'),
  `${JSON.stringify([{ column: 1, entries: [{ id: 'RT000001', slug: 'right-turn', name: 'Right turn' }] }])}\n`
);
await fs.writeFile(
  path.join(catalogBootstrapDir, 'progress.json'),
  `${JSON.stringify([{ date: '2026-06-03', summary: { totalMoves: 1 }, entries: [{ id: 'RT000001' }] }])}\n`
);
await fs.writeFile(path.join(catalogBootstrapDir, 'raw-moves.json'), `${JSON.stringify([{ id: 'RT000001', name: 'Right turn' }])}\n`);

test('catalog bootstrap JSON is imported into SQLite for runtime reads', async () => {
  const { getManifest, getMoves, getLayout, getProgressSnapshots, getRawMoveReference } = await import(
    '../src/lib/server/data.ts'
  );
  const { getAppDatabase } = await import('../src/lib/server/app-state.ts');

  assert.equal((await getManifest()).counts.moveRows, 1);
  assert.equal((await getMoves())[0].id, 'RT000001');
  assert.equal((await getLayout())[0].column, 1);
  assert.equal((await getProgressSnapshots())[0].date, '2026-06-03');
  assert.equal((await getRawMoveReference())[0].id, 'RT000001');

  assert.equal(getAppDatabase().prepare('SELECT COUNT(*) AS count FROM catalog_moves').get().count, 1);
  assert.equal(getAppDatabase().prepare('SELECT value FROM app_state_meta WHERE key = ?').get('catalog_json_bootstrap_v1').value, 'complete');
});
