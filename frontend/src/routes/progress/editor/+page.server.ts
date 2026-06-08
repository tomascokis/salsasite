import { requireAdminPage } from '$lib/server/auth-guard';
import { getLatestSnapshot, getLayout } from '$lib/server/data';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
  const [layout, snapshot] = await Promise.all([getLayout(), getLatestSnapshot()]);

  return {
    layout,
    snapshot
  };
};
