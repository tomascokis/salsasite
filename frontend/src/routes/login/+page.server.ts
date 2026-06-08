import { fail, redirect } from '@sveltejs/kit';
import {
  authenticateUser,
  createSession,
  sessionCookieMaxAge,
  sessionCookieName
} from '$lib/server/auth';
import type { Actions, PageServerLoad } from './$types';

function safeNext(value: string | null) {
  if (!value || !value.startsWith('/') || value.startsWith('//')) {
    return '/';
  }
  return value;
}

function secureCookie(url: URL) {
  return url.protocol === 'https:' || process.env.AUTH_COOKIE_SECURE === 'true';
}

export const load: PageServerLoad = async ({ locals, url }) => {
  const next = safeNext(url.searchParams.get('next'));
  if (locals.user) {
    throw redirect(303, next);
  }

  return { next };
};

export const actions: Actions = {
  default: async ({ request, cookies, url }) => {
    const formData = await request.formData();
    const username = String(formData.get('username') ?? '');
    const password = String(formData.get('password') ?? '');
    const next = safeNext(String(formData.get('next') ?? url.searchParams.get('next') ?? '/'));
    const user = await authenticateUser(username, password);
    if (!user) {
      return fail(400, {
        username,
        next,
        error: 'Invalid username or password.'
      });
    }

    const session = createSession(user.id);
    cookies.set(sessionCookieName, session.token, {
      httpOnly: true,
      sameSite: 'lax',
      secure: secureCookie(url),
      path: '/',
      maxAge: sessionCookieMaxAge()
    });

    throw redirect(303, next);
  }
};
