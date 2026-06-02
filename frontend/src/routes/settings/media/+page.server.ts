import { getMediaCatalogDiagnostics, listMediaManagerJobs } from '$lib/server/video-library';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async () => ({
  catalogDiagnostics: await getMediaCatalogDiagnostics(),
  jobs: listMediaManagerJobs(100)
});
