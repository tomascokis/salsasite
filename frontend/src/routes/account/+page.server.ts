import { fail } from '@sveltejs/kit';
import { changeOwnPassword } from '$lib/server/auth';
import { requireUser } from '$lib/server/auth-guard';
import type { Actions, PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  const user = requireUser(event);
  return {
    user: {
      username: user.username,
      role: user.role,
      lastLoginAt: user.lastLoginAt
    }
  };
};

export const actions: Actions = {
  password: async (event) => {
    const user = requireUser(event);
    const formData = await event.request.formData();
    const currentPassword = String(formData.get('currentPassword') ?? '');
    const nextPassword = String(formData.get('nextPassword') ?? '');
    const confirmPassword = String(formData.get('confirmPassword') ?? '');

    if (nextPassword !== confirmPassword) {
      return fail(400, {
        error: 'New passwords do not match.'
      });
    }

    try {
      await changeOwnPassword({
        userId: user.id,
        currentPassword,
        nextPassword
      });
    } catch (error) {
      return fail(400, {
        error: error instanceof Error ? error.message : 'Could not change password.'
      });
    }

    return {
      success: true,
      message: 'Password updated.'
    };
  }
};
