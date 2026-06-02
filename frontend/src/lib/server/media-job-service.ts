import { queuePosterGeneration } from './posters';
import {
  getMediaJobById,
  listMediaJobsWithFileActions
} from './media-manager';
import { queueSourceHash } from './media-source-service';
import { queueClipRender } from './media-render-service';

export function listMediaManagerJobs(limit = 100) {
  return listMediaJobsWithFileActions(limit);
}

export async function retryMediaManagerJob(jobId: string) {
  const job = getMediaJobById(jobId);
  if (!job) {
    throw new Error('Media job not found.');
  }

  if (job.type === 'clip.render') {
    return queueClipRender(job.targetId, { audit: false });
  }
  if (job.type === 'poster.generate') {
    await queuePosterGeneration(job.targetId);
    return getMediaJobById(job.id) ?? job;
  }
  if (job.type === 'source.hash') {
    return queueSourceHash(job.targetId);
  }

  throw new Error('This media job type cannot be retried from the UI.');
}
