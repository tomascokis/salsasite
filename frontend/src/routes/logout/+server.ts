import { redirect, type RequestHandler } from '@sveltejs/kit';
import { deleteSessionToken, sessionCookieName } from '$lib/server/auth';

function clearSession(cookies: Parameters<RequestHandler>[0]['cookies'], token: string | undefined) {
  deleteSessionToken(token);
  cookies.delete(sessionCookieName, { path: '/' });
}

export const POST: RequestHandler = async ({ cookies }) => {
  clearSession(cookies, cookies.get(sessionCookieName));
  throw redirect(303, '/login');
};

export const GET: RequestHandler = async ({ cookies }) => {
  clearSession(cookies, cookies.get(sessionCookieName));
  throw redirect(303, '/login');
};
