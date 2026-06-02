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

const env = await setupMediaTestEnvironment('media-render-service-boundaries');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('render service owns queue and cleanup while video-library remains a facade', async () => {
  const oldDraft = 'video-moves/RT000001 Source Clip abcdef12 draft old.mp4';
  const facadeOldDraft = 'video-moves/RT000001 Source Clip abcdef12 draft facade.mp4';
  const currentFull = 'video-moves/RT000001 Source Clip abcdef12.mp4';

  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 draft old.mp4'), 'old');
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 draft facade.mp4'), 'facade-old');
  await writeFile(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4'), 'current');

  const {
    cleanupObsoleteRenderedClipFiles,
    isClipRenderPending,
    queueClipRender
  } = await import('../src/lib/server/media-render-service.ts');
  const {
    cleanupObsoleteRenderedClipFiles: facadeCleanupObsoleteRenderedClipFiles,
    isClipRenderPending: facadeIsClipRenderPending,
    queueClipRender: facadeQueueClipRender
  } = await import('../src/lib/server/video-library.ts');
  const {
    completeMediaJob,
    createMediaCleanupJob,
    listMediaJobsWithFileActions,
    startMediaJob
  } = await import('../src/lib/server/media-manager.ts');
  const { listHistory } = await import('../src/lib/server/history.ts');

  const queuedJob = queueClipRender('missing-render-service-clip');
  assert.equal(queuedJob.type, 'clip.render');
  assert.equal(isClipRenderPending('missing-render-service-clip'), true);
  assert.equal(facadeIsClipRenderPending('missing-render-service-clip'), true);

  const facadeQueuedJob = facadeQueueClipRender('missing-render-facade-clip', { audit: false });
  assert.equal(facadeQueuedJob.type, 'clip.render');
  assert.equal(isClipRenderPending('missing-render-facade-clip'), true);

  const queueEntry = listHistory(20).find((entry) => entry.type === 'media.clip.render.queue');
  assert.ok(queueEntry);
  assert.equal(queueEntry.entityId, 'missing-render-service-clip');

  const serviceCleanupJob = createMediaCleanupJob({
    targetType: 'derivedClip',
    targetId: 'abcdef12-0000-4000-9000-000000000000',
    payload: { cleanupReason: 'obsolete-render-service-boundary' }
  });
  startMediaJob(serviceCleanupJob.id);
  const serviceDeleted = await cleanupObsoleteRenderedClipFiles({
    jobId: serviceCleanupJob.id,
    clipId: 'abcdef12-0000-4000-9000-000000000000',
    replacedRenderedFilePaths: [oldDraft, currentFull],
    currentRenderedFilePaths: [currentFull]
  });
  completeMediaJob(serviceCleanupJob.id);
  assert.deepEqual(serviceDeleted, [oldDraft]);

  const facadeCleanupJob = createMediaCleanupJob({
    targetType: 'derivedClip',
    targetId: 'abcdef12-0000-4000-9000-000000000000',
    payload: { cleanupReason: 'obsolete-render-facade-boundary' }
  });
  startMediaJob(facadeCleanupJob.id);
  const facadeDeleted = await facadeCleanupObsoleteRenderedClipFiles({
    jobId: facadeCleanupJob.id,
    clipId: 'abcdef12-0000-4000-9000-000000000000',
    replacedRenderedFilePaths: [facadeOldDraft, currentFull],
    currentRenderedFilePaths: [currentFull]
  });
  completeMediaJob(facadeCleanupJob.id);
  assert.deepEqual(facadeDeleted, [facadeOldDraft]);

  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 draft old.mp4')), false);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12 draft facade.mp4')), false);
  assert.equal(await exists(path.join(env.mediaRoot, 'RT000001 Source Clip abcdef12.mp4')), true);

  const detailedJobs = listMediaJobsWithFileActions(20);
  assert.ok(detailedJobs.find((job) => job.id === serviceCleanupJob.id));
  assert.ok(detailedJobs.find((job) => job.id === facadeCleanupJob.id));
  assert.equal(
    detailedJobs.some((job) =>
      job.fileActions.some((action) => action.actionType === 'cleanup-generated')
    ),
    true
  );
});
