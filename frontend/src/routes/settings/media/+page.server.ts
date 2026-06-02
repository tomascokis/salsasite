import { listMediaManagerJobs } from '$lib/server/video-library';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async () => ({
  jobs: listMediaManagerJobs(100)
});
