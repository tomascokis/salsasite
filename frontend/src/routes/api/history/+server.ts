import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth-guard';
import { listHistory } from '$lib/server/history';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async (event) => {
  requireAdmin(event);
  const { url } = event;
  return json({
    entries: listHistory(url.searchParams.get('limit') ?? 100)
  });
};
