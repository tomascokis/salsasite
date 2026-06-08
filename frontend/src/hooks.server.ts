import { error, redirect, type Handle } from '@sveltejs/kit';
import {
  getUserForSessionToken,
  pruneExpiredSessions,
  sessionCookieName
} from '$lib/server/auth';

const adminPagePrefixes = ['/media/edit', '/moves/create', '/progress/editor', '/settings'];
const adminApiPrefixes = ['/api/history', '/api/media/jobs', '/api/upload'];
const publicPrefixes = ['/login', '/_app', '/favicon'];
let lastPruneAt = 0;

function isPublicPath(pathname: string) {
  return publicPrefixes.some((prefix) => pathname === prefix || pathname.startsWith(`${prefix}/`));
}

function isApiOrAssetRequest(pathname: string) {
  return (
    pathname.startsWith('/api/') ||
    pathname.startsWith('/media/') ||
    pathname.startsWith('/posters/')
  );
}

function isAdminPage(pathname: string) {
  return (
    adminPagePrefixes.some((prefix) => pathname === prefix || pathname.startsWith(`${prefix}/`)) ||
    /^\/moves\/[^/]+\/edit\/?$/.test(pathname)
  );
}

function isAdminApi(pathname: string) {
  return adminApiPrefixes.some((prefix) => pathname === prefix || pathname.startsWith(`${prefix}/`));
}

function loginRedirect(pathname: string, search: string) {
  const next = `${pathname}${search}`;
  throw redirect(303, `/login?next=${encodeURIComponent(next)}`);
}

export const handle: Handle = async ({ event, resolve }) => {
  const now = Date.now();
  if (now - lastPruneAt > 60 * 60 * 1000) {
    pruneExpiredSessions();
    lastPruneAt = now;
  }

  const token = event.cookies.get(sessionCookieName);
  event.locals.user = getUserForSessionToken(token);
  const { pathname, search } = event.url;

  if (isPublicPath(pathname)) {
    return resolve(event);
  }

  if (!event.locals.user) {
    if (isApiOrAssetRequest(pathname)) {
      throw error(401, 'Authentication required.');
    }
    loginRedirect(pathname, search);
  }

  if (event.locals.user.role !== 'admin') {
    if (event.request.method !== 'GET' && event.request.method !== 'HEAD' && pathname !== '/logout') {
      throw error(403, 'Admin access required.');
    }
    if (isAdminPage(pathname) || isAdminApi(pathname)) {
      if (isApiOrAssetRequest(pathname)) {
        throw error(403, 'Admin access required.');
      }
      throw redirect(303, '/');
    }
  }

  return resolve(event);
};
