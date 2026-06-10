import { requireAdminPage } from '$lib/server/auth-guard';
import { getSecurityDashboard, unbanIpAddress } from '$lib/server/security-usage';
import type { Actions, PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
  return {
    dashboard: getSecurityDashboard()
  };
};

export const actions: Actions = {
  unban: async (event) => {
    const admin = requireAdminPage(event);
    const formData = await event.request.formData();
    const ipAddress = String(formData.get('ipAddress') ?? '');
    const reason = String(formData.get('reason') ?? 'Manual admin unban.');

    unbanIpAddress(ipAddress, admin.username, reason);
    return {
      success: true
    };
  }
};
