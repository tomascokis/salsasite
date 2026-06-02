import { json } from '@sveltejs/kit';
import { listHistory } from '$lib/server/history';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async ({ url }) => {
  const limit = Number(url.searchParams.get('limit') ?? 100);
  return json({
    entries: listHistory(Number.isFinite(limit) ? limit : 100)
  });
};

