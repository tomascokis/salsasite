import { requireAdminPage } from '$lib/server/auth-guard';
import { getSecurityDashboard } from '$lib/server/security-usage';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
  return {
    dashboard: getSecurityDashboard()
  };
};
