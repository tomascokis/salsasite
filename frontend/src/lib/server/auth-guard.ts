import { error, redirect, type RequestEvent } from '@sveltejs/kit';
import { isAdmin, type AuthUser } from './auth';

export function requireUser(event: RequestEvent): AuthUser {
  if (!event.locals?.user) {
    throw error(401, 'Authentication required.');
  }
  return event.locals.user;
}

export function requireAdmin(event: RequestEvent): AuthUser {
  const user = requireUser(event);
  if (!isAdmin(user)) {
    throw error(403, 'Admin access required.');
  }
  return user;
}

export function requireAdminPage(event: RequestEvent): AuthUser {
  const user = requireUser(event);
  if (!isAdmin(user)) {
    throw redirect(303, '/');
  }
  return user;
}
