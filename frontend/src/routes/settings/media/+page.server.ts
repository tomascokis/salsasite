import { requireAdminPage } from '$lib/server/auth-guard';
import { getMediaCatalogDiagnostics, listMediaManagerJobs } from '$lib/server/video-library';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
  return {
    catalogDiagnostics: await getMediaCatalogDiagnostics(),
    jobs: listMediaManagerJobs(100)
  };
};
