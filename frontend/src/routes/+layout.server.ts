import { isAdmin } from '$lib/server/auth';

export function load({ locals }) {
  return {
    user: locals.user
      ? {
          username: locals.user.username,
          role: locals.user.role
        }
      : null,
    isAdmin: isAdmin(locals.user)
  };
}
