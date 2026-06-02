import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
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

const env = await setupMediaTestEnvironment('media-catalog-repair');

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

test('catalog repair dry-run reports changes without writing catalog, jobs, history, or files', async () => {
  const stalePath = 'video-moves/RT000001 Source Clip abcdef12 low.mp4';
  await writeLibrary(env.libraryPath, {
    videoAssets: [
      sourceAsset(),
      moveAsset({
        id: 'stale-generated-asset',
        filePath: stalePath,
        originalFilename: path.basename(stalePath)
      })
    ],
    moveVideoLinks: [
      { id: 'stale-link', moveId: 'RT000001', assetId: 'stale-generated-asset', order: 0, createdAt: '2026-06-02T00:00:02.000Z' }
    ],
    derivedClips: [
      derivedClip({
        id: 'fedcba98-0000-4000-9000-000000000000',
        outputAssetId: null,
        publishedAssetId: null,
        lowResOutputFilePath: 'video-moves/missing low.mp4',
        lowResPaddedOutputFilePath: null,
        actionOutputFilePath: null,
        publishedLowResFilePath: null,
        publishedLowResPaddedFilePath: null,
        publishedActionOutputFilePath: null
      })
    ]
  });
  await writeFile(path.join(env.mediaRoot, 'RT000001 Basic.mp4'), 'legacy move video');
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4'), 'stale generated');
  const before = await fs.readFile(env.libraryPath, 'utf-8');

  const { scanMediaCatalogRepairs } = await import('../src/lib/server/media-repair-service.ts');
  const { listMediaJobsWithFileActions } = await import('../src/lib/server/media-manager.ts');
  const { listHistory } = await import('../src/lib/server/history.ts');

  const repair = await scanMediaCatalogRepairs([move()]);

  assert.equal(repair.dryRun, true);
  assert.equal(repair.changed, true);
  assert.equal(repair.summary.legacyMoveAssetsAdded, 1);
  assert.equal(repair.summary.legacyLinksAdded, 1);
  assert.equal(repair.summary.staleGeneratedAssetsRemoved, 1);
  assert.equal(repair.summary.staleGeneratedFilesTrashed, 1);
  assert.equal(repair.summary.missingVariantPathsPruned, 1);
  assert.equal(await fs.readFile(env.libraryPath, 'utf-8'), before);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4')), true);
  assert.equal(listMediaJobsWithFileActions().length, 0);
  assert.equal(listHistory().some((entry) => entry.type === 'media.catalog.repair'), false);
});

test('catalog repair apply writes catalog, trash-backed file actions, and audit history', async () => {
  const { runMediaCatalogRepairs } = await import('../src/lib/server/media-repair-service.ts');
  const { listMediaJobsWithFileActions } = await import('../src/lib/server/media-manager.ts');
  const { listHistory } = await import('../src/lib/server/history.ts');

  const repair = await runMediaCatalogRepairs([move()]);

  assert.equal(repair.dryRun, false);
  assert.equal(repair.changed, true);
  assert.equal(repair.historyActionId?.length > 0, true);
  assert.equal(repair.mediaJobIds.length, 1);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4')), false);

  const library = await readLibrary(env.libraryPath);
  const legacyAsset = library.videoAssets.find((asset) => asset.filePath === 'video-moves/RT000001 Basic.mp4');
  assert.ok(legacyAsset);
  assert.equal(library.moveVideoLinks.some((link) => link.moveId === 'RT000001' && link.assetId === legacyAsset.id), true);
  assert.equal(library.videoAssets.some((asset) => asset.id === 'stale-generated-asset'), false);
  const repairedClip = library.derivedClips.find((clip) => clip.id === 'fedcba98-0000-4000-9000-000000000000');
  assert.equal(repairedClip.lowResOutputFilePath, null);

  const cleanupJob = listMediaJobsWithFileActions().find((job) => repair.mediaJobIds.includes(job.id));
  assert.ok(cleanupJob);
  assert.equal(cleanupJob.type, 'file.cleanup');
  assert.equal(cleanupJob.fileActions.some((action) => action.actionType === 'cleanup-generated' && action.backupPath), true);

  const historyEntry = listHistory().find((entry) => entry.id === repair.historyActionId);
  assert.ok(historyEntry);
  assert.equal(historyEntry.type, 'media.catalog.repair');
  assert.equal(historyEntry.canUndo, false);
});

test('compatibility repair path still repairs without recording catalog repair history', async () => {
  await writeLibrary(env.libraryPath, {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });
  await writeFile(path.join(env.mediaRoot, 'RT000003 Compatibility.mp4'), 'legacy move video');

  const { getVideoLibraryWithRepairs } = await import('../src/lib/server/video-library.ts');
  const { listHistory } = await import('../src/lib/server/history.ts');
  const beforeHistoryCount = listHistory().filter((entry) => entry.type === 'media.catalog.repair').length;

  const library = await getVideoLibraryWithRepairs([move({ id: 'RT000003', slug: 'rt000003', name: 'Compatibility' })]);

  assert.equal(library.videoAssets.some((asset) => asset.filePath === 'video-moves/RT000003 Compatibility.mp4'), true);
  assert.equal(listHistory().filter((entry) => entry.type === 'media.catalog.repair').length, beforeHistoryCount);
});

test('catalog repair dry-run and apply report orphan draft relinks and generated renames', async () => {
  const draftMoveId = 'FANCYDRAFT';
  const nextMoveId = 'RT000002';
  const clipId = '12345678-0000-4000-9000-000000000000';
  const oldPath = 'video-moves/FANCYDRAFT Source Clip 12345678.mp4';
  const nextPath = 'video-moves/RT000002 Source Clip 12345678.mp4';

  await writeLibrary(env.libraryPath, {
    videoAssets: [
      sourceAsset(),
      moveAsset({
        id: 'orphan-output-asset',
        filePath: oldPath,
        originalFilename: path.basename(oldPath)
      })
    ],
    moveVideoLinks: [
      { id: 'orphan-link', moveId: draftMoveId, assetId: 'orphan-output-asset', order: 0, createdAt: '2026-06-02T00:00:02.000Z' }
    ],
    derivedClips: [
      derivedClip({
        id: clipId,
        moveId: draftMoveId,
        moveDisplayId: draftMoveId,
        outputAssetId: 'orphan-output-asset',
        publishedAssetId: 'orphan-output-asset',
        actionOutputFilePath: null,
        lowResOutputFilePath: null,
        lowResPaddedOutputFilePath: null,
        publishedActionOutputFilePath: null,
        publishedLowResFilePath: null,
        publishedLowResPaddedFilePath: null
      })
    ]
  });
  await writeFile(path.join(env.mediaRoot, 'FANCYDRAFT Source Clip 12345678.mp4'), 'generated output');

  const { scanMediaCatalogRepairs, runMediaCatalogRepairs } = await import('../src/lib/server/media-repair-service.ts');
  const moves = [move({ id: nextMoveId, slug: 'rt000002', name: 'Fancy Draft' })];
  const scan = await scanMediaCatalogRepairs(moves);

  assert.equal(scan.dryRun, true);
  assert.equal(scan.summary.orphanDraftMoveIdsRelinked, 1);
  assert.equal(scan.summary.generatedRenameActions, 1);
  assert.deepEqual(scan.changes.generatedRenameActions.map((entry) => [entry.fromPath, entry.toPath]), [[oldPath, nextPath]]);
  assert.equal(await exists(path.join(env.mediaRoot, 'FANCYDRAFT Source Clip 12345678.mp4')), true);

  const applied = await runMediaCatalogRepairs(moves);
  const library = await readLibrary(env.libraryPath);
  const clip = library.derivedClips.find((entry) => entry.id === clipId);
  const asset = library.videoAssets.find((entry) => entry.id === 'orphan-output-asset');

  assert.equal(applied.summary.orphanDraftMoveIdsRelinked, 1);
  assert.equal(applied.summary.generatedRenameActions, 1);
  assert.equal(clip.moveId, nextMoveId);
  assert.equal(clip.moveDisplayId, nextMoveId);
  assert.equal(asset.filePath, nextPath);
  assert.equal(library.moveVideoLinks.some((link) => link.id === 'orphan-link' && link.moveId === nextMoveId), true);
  assert.equal(await exists(path.join(env.mediaRoot, 'FANCYDRAFT Source Clip 12345678.mp4')), false);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000002 Source Clip 12345678.mp4')), true);
});
