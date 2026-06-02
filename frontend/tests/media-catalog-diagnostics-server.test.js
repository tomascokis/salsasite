import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  derivedClip,
  moveAsset,
  setupMediaTestEnvironment,
  sourceAsset,
  writeFile
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-catalog-diagnostics');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

function moveLink(overrides = {}) {
  return {
    id: 'link-1',
    moveId: 'RT000001',
    assetId: 'output-asset',
    order: 0,
    createdAt: '2026-06-02T00:00:02.000Z',
    ...overrides
  };
}

test('media catalog diagnostics report bootstrap state, counts, latest export, and clean integrity', async () => {
  const {
    exportMediaCatalogSnapshot,
    writeMediaCatalog
  } = await import('../src/lib/server/media-catalog.ts');
  const { getMediaCatalogDiagnostics } = await import('../src/lib/server/media-catalog-diagnostics.ts');

  await writeFile(path.join(env.sourceRoot, 'source.mp4'), 'source');
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4'), 'move');
  await writeMediaCatalog({
    videoAssets: [
      sourceAsset({ filePath: 'video-sources/source.mp4' }),
      moveAsset({ filePath: 'video-moves/RT000001 Source Clip abcdef12.mp4' })
    ],
    moveVideoLinks: [moveLink()],
    derivedClips: [
      derivedClip({
        outputAssetId: null,
        actionOutputFilePath: null,
        lowResOutputFilePath: null,
        lowResPaddedOutputFilePath: null,
        publishedAssetId: null,
        publishedActionOutputFilePath: null,
        publishedLowResFilePath: null,
        publishedLowResPaddedFilePath: null
      })
    ]
  });

  await exportMediaCatalogSnapshot(new Date('2026-06-03T04:05:06.789Z'));
  const diagnostics = await getMediaCatalogDiagnostics();

  assert.equal(diagnostics.database.pathLabel, 'DATA_DIR/app-state.sqlite');
  assert.equal(diagnostics.database.bootstrapStatus, 'complete');
  assert.equal(diagnostics.database.bootstrapMetaValue, 'complete');
  assert.deepEqual(diagnostics.counts, {
    videoAssets: 2,
    sourceAssets: 1,
    moveAssets: 1,
    moveVideoLinks: 1,
    derivedClips: 1,
    clipsByStatus: {
      pending: 0,
      rendering: 0,
      ready: 1,
      failed: 0
    }
  });
  assert.equal(diagnostics.latestExport?.filePath, 'media-catalog-exports/video-library-2026-06-03T04-05-06-789Z.json');
  assert.equal(diagnostics.latestExport?.exportedAt, '2026-06-03T04:05:06.789Z');
  assert.equal(diagnostics.integritySummary.total, 0);
  assert.deepEqual(diagnostics.findings, []);
});

test('media catalog diagnostics report missing files, missing references, and duplicate normalized paths', async () => {
  const { writeMediaCatalog } = await import('../src/lib/server/media-catalog.ts');
  const { getMediaCatalogDiagnostics } = await import('../src/lib/server/media-catalog-diagnostics.ts');
  const { getAppDatabase } = await import('../src/lib/server/app-state.ts');

  await writeMediaCatalog({
    videoAssets: [
      sourceAsset({ filePath: 'video-sources/missing-source.mp4' }),
      moveAsset({ filePath: 'video-moves/missing-move.mp4' })
    ],
    moveVideoLinks: [moveLink()],
    derivedClips: [
      derivedClip({
        outputAssetId: 'missing-output-asset',
        publishedAssetId: 'missing-published-asset',
        actionOutputFilePath: 'video-moves/missing-action.mp4',
        lowResOutputFilePath: 'video-moves/missing-low.mp4',
        lowResPaddedOutputFilePath: null,
        publishedActionOutputFilePath: null,
        publishedLowResFilePath: null,
        publishedLowResPaddedFilePath: null
      })
    ]
  });

  const db = getAppDatabase();
  db.exec('PRAGMA foreign_keys = OFF');
  try {
    db.prepare(`
      INSERT INTO media_move_video_links (id, move_id, asset_id, sort_order, created_at)
      VALUES (?, ?, ?, ?, ?)
    `).run('missing-link-asset', 'RT000002', 'missing-link-target', 0, '2026-06-02T00:00:04.000Z');
    db.prepare('DELETE FROM media_video_assets WHERE id = ?').run('source-asset');
    db.prepare(`
      INSERT INTO media_video_assets (
        id, kind, file_path, display_name, original_filename, dancers_json, timing, content_type,
        environment, origin_type, tags_json, hash_status, created_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `).run(
      'legacy-duplicate-path',
      'move',
      'videomoves/missing-move.mp4',
      'Legacy Duplicate',
      'missing-move.mp4',
      '[]',
      'other',
      'other',
      'class',
      'self-recorded',
      '[]',
      'pending',
      '2026-06-02T00:00:05.000Z'
    );
  } finally {
    db.exec('PRAGMA foreign_keys = ON');
  }

  const diagnostics = await getMediaCatalogDiagnostics();
  const types = diagnostics.findings.map((finding) => finding.type);

  assert.equal(types.includes('missing-asset-file'), true);
  assert.equal(types.includes('missing-generated-file'), true);
  assert.equal(types.includes('missing-clip-output-asset'), true);
  assert.equal(types.includes('missing-move-link-asset'), true);
  assert.equal(types.includes('missing-clip-source-asset'), true);
  assert.equal(types.includes('duplicate-asset-file-path'), true);
  assert.equal(diagnostics.integritySummary.errors > 0, true);
  assert.equal(diagnostics.integritySummary.warnings > 0, true);
  assert.equal(JSON.stringify(diagnostics).includes(env.tempRoot), false);
});
