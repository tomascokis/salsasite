import { json } from '@sveltejs/kit';
import { listHistory } from '$lib/server/history';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async ({ url }) => {
  return json({
    entries: listHistory(url.searchParams.get('limit') ?? 100)
  });
};
