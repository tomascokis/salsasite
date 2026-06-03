import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'salsa-media-manager-'));
const dataDir = path.join(tempRoot, 'data');
const appStateBootstrapDir = path.join(dataDir, 'bootstrap', 'app-state');
const mediaRoot = path.join(tempRoot, 'video-moves');
const sourceRoot = path.join(tempRoot, 'video-sources');
const posterRoot = path.join(tempRoot, 'video-posters');

process.env.DATA_DIR = dataDir;
process.env.MEDIA_ROOT = mediaRoot;
process.env.SOURCE_ROOT = sourceRoot;
process.env.POSTER_ROOT = posterRoot;

await Promise.all([
  fs.mkdir(appStateBootstrapDir, { recursive: true }),
  fs.mkdir(mediaRoot),
  fs.mkdir(sourceRoot),
  fs.mkdir(posterRoot)
]);

test.after(async () => {
  await fs.rm(tempRoot, { recursive: true, force: true });
});

async function exists(filePath) {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

async function writeFile(filePath, contents = 'media') {
  await fs.mkdir(path.dirname(filePath), { recursive: true });
  await fs.writeFile(filePath, contents);
}

test('media manager records managed trash, rename, and temp delete file actions', async () => {
  const {
    completeMediaJob,
    createMediaCleanupJob,
    deleteTemporaryFile,
    listMediaFileActionsForJob,
    listMediaJobsWithFileActions,
    renameManagedVideoFiles,
    startMediaJob,
    trashManagedVideoFileSet
  } = await import('../src/lib/server/media-manager.ts');

  await writeFile(path.join(mediaRoot, 'old.mp4'));
  await writeFile(path.join(posterRoot, 'video-moves', 'old.jpg'), 'poster');
  await writeFile(path.join(sourceRoot, 'temporary.mp4'));

  const job = createMediaCleanupJob({
    targetType: 'test',
    targetId: 'media-manager-actions',
    payload: { test: true }
  });
  startMediaJob(job.id);

  await renameManagedVideoFiles({
    jobId: job.id,
    fromPath: 'video-moves/old.mp4',
    toPath: 'video-moves/new.mp4',
    metadata: { reason: 'test-rename' }
  });
  await trashManagedVideoFileSet({
    jobId: job.id,
    filePaths: ['video-moves/new.mp4'],
    actionType: 'cleanup-generated',
    metadata: { reason: 'test-trash' }
  });
  await deleteTemporaryFile({
    jobId: job.id,
    absolutePath: path.join(sourceRoot, 'temporary.mp4'),
    filePath: 'video-sources/temporary.mp4',
    metadata: { reason: 'duplicate-upload' }
  });
  completeMediaJob(job.id);

  assert.equal(await exists(path.join(mediaRoot, 'old.mp4')), false);
  assert.equal(await exists(path.join(mediaRoot, 'new.mp4')), false);
  assert.equal(await exists(path.join(dataDir, 'media-trash', job.id, 'video-moves', 'new.mp4')), true);
  assert.equal(await exists(path.join(dataDir, 'media-trash', job.id, 'video-posters', 'video-moves', 'new.jpg')), true);
  assert.equal(await exists(path.join(sourceRoot, 'temporary.mp4')), false);

  const actions = listMediaFileActionsForJob(job.id);
  assert.equal(actions.some((action) => action.actionType === 'rename-video' && action.filePath === 'video-moves/old.mp4'), true);
  assert.equal(actions.some((action) => action.actionType === 'rename-poster' && action.filePath === 'video-posters/video-moves/old.jpg'), true);
  assert.equal(actions.some((action) => action.actionType === 'cleanup-generated' && action.filePath === 'video-moves/new.mp4'), true);
  assert.equal(actions.some((action) => action.actionType === 'delete-temp' && action.filePath === 'video-sources/temporary.mp4'), true);

  const detailedJob = listMediaJobsWithFileActions(10).find((entry) => entry.id === job.id);
  assert.ok(detailedJob);
  assert.equal(detailedJob.fileActionSummary.total, detailedJob.fileActions.length);
  assert.equal(detailedJob.fileActionSummary.byActionType['rename-video'], 1);
  assert.equal(detailedJob.fileActionSummary.byActionType['delete-temp'], 1);
  assert.equal(detailedJob.fileActionSummary.missing > 0, true);
  assert.equal(JSON.stringify(detailedJob.fileActions).includes('originalAbsolutePath'), false);
  assert.equal(JSON.stringify(detailedJob.fileActions).includes('destinationAbsolutePath'), false);
});

test('source delete records media actions and history undo restores catalog and files', async () => {
  const libraryPath = path.join(appStateBootstrapDir, 'video-library.json');
  const sourceFile = path.join(sourceRoot, 'source.mp4');
  const paddedFile = path.join(mediaRoot, 'RT000001 Source Clip abcdef12.mp4');
  const actionFile = path.join(mediaRoot, 'RT000001 Source Clip abcdef12 action.mp4');
  const lowFile = path.join(mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4');

  await writeFile(sourceFile, 'source');
  await writeFile(paddedFile, 'padded');
  await writeFile(actionFile, 'action');
  await writeFile(lowFile, 'low');
  await writeFile(path.join(posterRoot, 'video-sources', 'source.jpg'), 'source-poster');
  await writeFile(path.join(posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12.jpg'), 'padded-poster');

  await fs.writeFile(
    libraryPath,
    `${JSON.stringify(
      {
        videoAssets: [
          {
            id: 'source-asset',
            kind: 'source',
            filePath: 'video-sources/source.mp4',
            displayName: 'Source Clip',
            originalFilename: 'source.mp4',
            dancers: ['A'],
            timing: 'on1',
            contentType: 'music',
            environment: 'class',
            originType: 'self-recorded',
            recordDate: null,
            classWorkshop: null,
            tags: [],
            notes: null,
            contentHash: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
            contentHashAlgorithm: 'sha256',
            contentSizeBytes: 6,
            hashStatus: 'ready',
            createdAt: '2026-06-02T00:00:00.000Z'
          },
          {
            id: 'output-asset',
            kind: 'move',
            filePath: 'video-moves/RT000001 Source Clip abcdef12.mp4',
            displayName: 'Source Clip',
            originalFilename: 'RT000001 Source Clip abcdef12.mp4',
            dancers: ['A'],
            timing: 'on1',
            contentType: 'music',
            environment: 'class',
            originType: 'self-recorded',
            recordDate: null,
            classWorkshop: null,
            tags: [],
            notes: null,
            contentHash: null,
            contentHashAlgorithm: null,
            contentSizeBytes: null,
            hashStatus: 'pending',
            createdAt: '2026-06-02T00:00:01.000Z'
          }
        ],
        moveVideoLinks: [
          {
            id: 'link-1',
            moveId: 'RT000001',
            assetId: 'output-asset',
            order: 0,
            createdAt: '2026-06-02T00:00:02.000Z'
          }
        ],
        derivedClips: [
          {
            id: 'abcdef12-0000-4000-9000-000000000000',
            sourceAssetId: 'source-asset',
            moveId: 'RT000001',
            moveDisplayId: 'RT000001',
            isKeyVideo: true,
            label: 'Source Clip',
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
            countTimingPreset: 'on2-default',
            outputAssetId: 'output-asset',
            actionOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 action.mp4',
            lowResOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 low.mp4',
            lowResPaddedOutputFilePath: null,
            publishedAssetId: 'output-asset',
            publishedActionOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 action.mp4',
            publishedLowResFilePath: 'video-moves/RT000001 Source Clip abcdef12 low.mp4',
            publishedLowResPaddedFilePath: null,
            publishedAt: '2026-06-02T00:00:03.000Z',
            status: 'ready',
            error: null,
            createdAt: '2026-06-02T00:00:03.000Z',
            updatedAt: '2026-06-02T00:00:03.000Z'
          }
        ]
      },
      null,
      2
    )}\n`
  );

  const { deleteSourceAsset } = await import('../src/lib/server/video-library.ts');
  const { readMediaCatalog } = await import('../src/lib/server/media-catalog.ts');
  const { listHistory, undoAction } = await import('../src/lib/server/history.ts');

  const result = await deleteSourceAsset('source-asset');
  assert.deepEqual(result.deletedAssetIds.sort(), ['output-asset', 'source-asset']);
  assert.deepEqual(result.deletedClipIds, ['abcdef12-0000-4000-9000-000000000000']);
  assert.equal(await exists(sourceFile), false);
  assert.equal(await exists(paddedFile), false);
  assert.equal(await exists(actionFile), false);

  const deletedLibrary = await readMediaCatalog();
  assert.equal(deletedLibrary.videoAssets.length, 0);
  assert.equal(deletedLibrary.derivedClips.length, 0);
  assert.equal(deletedLibrary.moveVideoLinks.length, 0);

  const deleteAction = listHistory(10).find((entry) => entry.type === 'media.source.delete');
  assert.ok(deleteAction);
  assert.equal(deleteAction.canUndo, true);
  await undoAction(deleteAction.id);

  const restoredLibrary = await readMediaCatalog();
  assert.equal(restoredLibrary.videoAssets.length, 2);
  assert.equal(restoredLibrary.derivedClips.length, 1);
  assert.equal(restoredLibrary.moveVideoLinks.length, 1);
  assert.equal(await exists(sourceFile), true);
  assert.equal(await exists(paddedFile), true);
  assert.equal(await exists(actionFile), true);
  assert.equal(await exists(lowFile), true);
});
