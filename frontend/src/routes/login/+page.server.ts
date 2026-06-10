import { fail, redirect } from '@sveltejs/kit';
import {
  authenticateUser,
  createSession,
  sessionCookieMaxAge,
  sessionCookieName
} from '$lib/server/auth';
import {
  clientIpFromEvent,
  evaluateIpBanAfterLoginAttempt,
  evaluateIpBanAfterLoginView,
  getActiveIpBan,
  recordLoginAttempt,
  recordLoginView
} from '$lib/server/security-usage';
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

export const load: PageServerLoad = async (event) => {
  const { locals, url } = event;
  const next = safeNext(url.searchParams.get('next'));
  if (locals.user) {
    throw redirect(303, next);
  }

  const ipAddress = clientIpFromEvent(event);
  if (getActiveIpBan(ipAddress)) {
    return {
      next,
      banned: true,
      error: 'Too many login attempts. Try again later.'
    };
  }

  try {
    recordLoginView({ ipAddress });
    const ban = evaluateIpBanAfterLoginView(ipAddress);
    if (ban) {
      return {
        next,
        banned: true,
        error: 'Too many login attempts. Try again later.'
      };
    }
  } catch (error) {
    console.warn('Failed to record login view', error);
  }

  return { next, banned: false, error: null };
};

export const actions: Actions = {
  default: async (event) => {
    const { request, cookies, url } = event;
    const formData = await request.formData();
    const username = String(formData.get('username') ?? '');
    const password = String(formData.get('password') ?? '');
    const next = safeNext(String(formData.get('next') ?? url.searchParams.get('next') ?? '/'));
    const ipAddress = clientIpFromEvent(event);

    if (getActiveIpBan(ipAddress)) {
      return fail(429, {
        username,
        next,
        banned: true,
        error: 'Too many login attempts. Try again later.'
      });
    }

    const user = await authenticateUser(username, password);
    try {
      recordLoginAttempt({
        username,
        userId: user?.id ?? null,
        ipAddress,
        success: Boolean(user)
      });
      if (!user) {
        evaluateIpBanAfterLoginAttempt(ipAddress);
      }
    } catch (error) {
      console.warn('Failed to record login attempt', error);
    }

    if (!user) {
      return fail(400, {
        username,
        next,
        banned: false,
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
