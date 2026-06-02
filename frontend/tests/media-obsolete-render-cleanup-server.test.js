import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';
import {
  cleanupMediaTestEnvironment,
  exists,
  setupMediaTestEnvironment,
  writeFile
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('obsolete-render-cleanup');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('cleanupObsoleteRenderedClipFiles trashes obsolete generated files and keeps current files', async () => {
  const oldDraft = 'video-moves/RT000001 Source Clip abcdef12 draft old.mp4';
  const currentFull = 'video-moves/RT000001 Source Clip abcdef12.mp4';
  const currentLow = 'video-moves/RT000001 Source Clip abcdef12 low.mp4';

  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 draft old.mp4'), 'old');
  await writeFile(path.join(env.posterRoot, 'video-moves', 'RT000001 Source Clip abcdef12 draft old.jpg'), 'old-poster');
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4'), 'current');
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4'), 'current-low');

  const { cleanupObsoleteRenderedClipFiles } = await import('../src/lib/server/video-library.ts');
  const { completeMediaJob, createMediaCleanupJob, listMediaJobsWithFileActions, startMediaJob } = await import('../src/lib/server/media-manager.ts');

  const job = createMediaCleanupJob({
    targetType: 'derivedClip',
    targetId: 'abcdef12-0000-4000-9000-000000000000',
    payload: { cleanupReason: 'obsolete-render-test' }
  });
  startMediaJob(job.id);
  const deletedFilePaths = await cleanupObsoleteRenderedClipFiles({
    jobId: job.id,
    clipId: 'abcdef12-0000-4000-9000-000000000000',
    replacedRenderedFilePaths: [oldDraft, currentLow],
    currentRenderedFilePaths: [currentFull, currentLow]
  });
  completeMediaJob(job.id);

  assert.deepEqual(deletedFilePaths, [oldDraft]);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 draft old.mp4')), false);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4')), true);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 low.mp4')), true);
  assert.equal(await exists(path.join(env.dataDir, 'media-trash', job.id, 'video-moves', 'RT000001 Source Clip abcdef12 draft old.mp4')), true);
  assert.equal(
    await exists(path.join(env.dataDir, 'media-trash', job.id, 'video-posters', 'video-moves', 'RT000001 Source Clip abcdef12 draft old.jpg')),
    true
  );

  const detailedJob = listMediaJobsWithFileActions(10).find((entry) => entry.id === job.id);
  assert.ok(detailedJob);
  assert.equal(
    detailedJob.fileActions.some((action) =>
      action.actionType === 'cleanup-generated' &&
      action.metadata &&
      typeof action.metadata === 'object' &&
      action.metadata.cleanupReason === 'obsolete-render'
    ),
    true
  );
});
