import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment,
  sourceAsset,
  writeFile,
  writeLibrary
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-job-service-boundaries');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('job service lists and retries supported media jobs like the video-library facade', async () => {
  await writeLibrary(env.libraryPath, {
    videoAssets: [
      sourceAsset({
        contentHash: null,
        contentHashAlgorithm: null,
        contentSizeBytes: null,
        hashStatus: 'pending'
      })
    ],
    moveVideoLinks: [],
    derivedClips: []
  });
  await writeFile(path.join(env.sourceRoot, 'source.mp4'), 'source bytes');
  await writeFile(path.join(env.mediaRoot, 'poster-target.mp4'), 'video bytes');
  await writeFile(path.join(env.posterRoot, 'video-moves', 'poster-target.jpg'), 'poster');

  const jobService = await import('../src/lib/server/media-job-service.ts');
  const facade = await import('../src/lib/server/video-library.ts');
  const { queueSourceHash } = await import('../src/lib/server/media-source-service.ts');
  const { queueClipRender } = await import('../src/lib/server/media-render-service.ts');
  const { queuePosterGeneration } = await import('../src/lib/server/posters.ts');

  const renderJob = queueClipRender('missing-job-service-render-clip', { audit: false });
  const retriedRenderJob = await jobService.retryMediaManagerJob(renderJob.id);
  assert.equal(retriedRenderJob.type, 'clip.render');
  const facadeRetriedRenderJob = await facade.retryMediaManagerJob(renderJob.id);
  assert.equal(facadeRetriedRenderJob.type, 'clip.render');

  const sourceHashJob = await queueSourceHash('source-asset');
  const retriedSourceHashJob = await jobService.retryMediaManagerJob(sourceHashJob.id);
  assert.equal(retriedSourceHashJob.type, 'source.hash');

  await queuePosterGeneration('video-moves/poster-target.mp4');
  const posterJob = jobService.listMediaManagerJobs(20).find((job) => job.type === 'poster.generate');
  assert.ok(posterJob);
  const retriedPosterJob = await jobService.retryMediaManagerJob(posterJob.id);
  assert.equal(retriedPosterJob.type, 'poster.generate');

  assert.deepEqual(jobService.listMediaManagerJobs(20), facade.listMediaManagerJobs(20));
});
