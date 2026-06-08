import { requireAdminPage } from '$lib/server/auth-guard';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
  return {};
};
