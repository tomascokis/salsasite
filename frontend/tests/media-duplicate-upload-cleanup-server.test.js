import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';
import {
  cleanupMediaTestEnvironment,
  exists,
  readLibrary,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('duplicate-upload-cleanup');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('createSourceAsset records duplicate upload temp cleanup', async () => {
  const { createSourceAsset } = await import('../src/lib/server/video-library.ts');
  const { listMediaJobsWithFileActions } = await import('../src/lib/server/media-manager.ts');

  const input = {
    originalFilename: 'source.mp4',
    displayName: 'Source Clip',
    dancers: ['A'],
    timing: 'on1',
    contentType: 'music',
    environment: 'class',
    originType: 'self-recorded',
    recordDate: null,
    classWorkshop: null,
    tags: [],
    notes: null
  };

  const first = await createSourceAsset({
    ...input,
    fileBuffer: Buffer.from('same-video-bytes')
  });
  const second = await createSourceAsset({
    ...input,
    fileBuffer: Buffer.from('same-video-bytes')
  });

  assert.equal(second.reusedExisting, true);
  assert.equal(second.asset.id, first.asset.id);
  assert.equal(await exists(path.join(env.sourceRoot, 'source.mp4')), true);
  assert.equal(await exists(path.join(env.sourceRoot, 'source 2.mp4')), false);

  const library = await readLibrary(env.libraryPath);
  assert.equal(library.videoAssets.length, 1);

  const cleanupJob = listMediaJobsWithFileActions(20).find((job) =>
    job.fileActions.some((action) => action.actionType === 'delete-temp')
  );
  assert.ok(cleanupJob);
  const deleteAction = cleanupJob.fileActions.find((action) => action.actionType === 'delete-temp');
  assert.equal(deleteAction.filePath, 'video-sources/source 2.mp4');
  assert.equal(JSON.stringify(deleteAction.metadata).includes('originalAbsolutePath'), false);
});
