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

const env = await setupMediaTestEnvironment('media-catalog-repair-api');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

function requestFor(action) {
  return new Request('http://test.local/api/media/jobs', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ action })
  });
}

function move(overrides = {}) {
  return {
    id: 'RT000001',
    displayId: null,
    slug: 'rt000001',
    name: 'API Legacy',
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

test('media jobs API scans and runs catalog repairs through existing endpoint', async () => {
  const testMove = move();
  await fs.writeFile(path.join(env.dataDir, 'moves.json'), `${JSON.stringify([testMove], null, 2)}\n`);

  await writeLibrary(env.libraryPath, {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });
  await writeFile(path.join(env.mediaRoot, `${testMove.id} API Legacy.mp4`), 'legacy move video');

  const { POST } = await import('../src/routes/api/media/jobs/+server.ts');
  const scanResponse = await POST({ request: requestFor('catalog.repair.scan') });
  const scanPayload = await scanResponse.json();

  assert.equal(scanResponse.status, 200);
  assert.equal(scanPayload.ok, true);
  assert.equal(scanPayload.repair.dryRun, true);
  assert.equal(scanPayload.repair.changed, true);
  assert.equal(scanPayload.repair.summary.legacyMoveAssetsAdded, 1);

  const runResponse = await POST({ request: requestFor('catalog.repair.run') });
  const runPayload = await runResponse.json();

  assert.equal(runResponse.status, 200);
  assert.equal(runPayload.ok, true);
  assert.equal(runPayload.repair.dryRun, false);
  assert.equal(runPayload.repair.changed, true);
  assert.equal(runPayload.repair.historyActionId.length > 0, true);

  const { GET: historyGet } = await import('../src/routes/api/history/+server.ts');
  const historyResponse = await historyGet({ url: new URL('http://test.local/api/history?limit=20') });
  const historyPayload = await historyResponse.json();
  const repairEntry = historyPayload.entries.find((entry) => entry.id === runPayload.repair.historyActionId);

  assert.ok(repairEntry);
  assert.equal(repairEntry.type, 'media.catalog.repair');
  assert.equal(repairEntry.canUndo, false);
});

test('media jobs API exports a JSON snapshot of the SQLite catalog', async () => {
  const { POST } = await import('../src/routes/api/media/jobs/+server.ts');
  const response = await POST({ request: requestFor('catalog.export.json') });
  const payload = await response.json();

  assert.equal(response.status, 200);
  assert.equal(payload.ok, true);
  assert.equal(payload.export.filePath.startsWith('media-catalog-exports/video-library-'), true);
  assert.equal(payload.export.filePath.endsWith('.json'), true);
  assert.equal('absolutePath' in payload.export, false);
  assert.equal(payload.export.counts.videoAssets > 0, true);

  const exportedPath = path.join(env.dataDir, payload.export.filePath);
  const exported = JSON.parse(await fs.readFile(exportedPath, 'utf-8'));
  assert.equal(Array.isArray(exported.videoAssets), true);
  assert.equal(Array.isArray(exported.moveVideoLinks), true);
  assert.equal(Array.isArray(exported.derivedClips), true);
});

test('media jobs API returns read-only catalog diagnostics', async () => {
  const { POST } = await import('../src/routes/api/media/jobs/+server.ts');
  const response = await POST({ request: requestFor('catalog.diagnostics.scan') });
  const payload = await response.json();

  assert.equal(response.status, 200);
  assert.equal(payload.ok, true);
  assert.equal(payload.diagnostics.database.pathLabel, 'DATA_DIR/app-state.sqlite');
  assert.equal(typeof payload.diagnostics.counts.videoAssets, 'number');
  assert.equal(Array.isArray(payload.diagnostics.findings), true);
  assert.equal(JSON.stringify(payload.diagnostics).includes(env.tempRoot), false);
});

test('media jobs API still returns clear 400 for unsupported actions', async () => {
  const { POST } = await import('../src/routes/api/media/jobs/+server.ts');
  const response = await POST({ request: requestFor('unsupported.action') });
  const payload = await response.json();

  assert.equal(response.status, 400);
  assert.equal(payload.error, 'Unsupported media job action.');
});
