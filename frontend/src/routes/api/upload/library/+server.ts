import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth-guard';
import { getMoves } from '$lib/server/data';
import { getUploadPageData } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async (event) => {
  requireAdmin(event);
  const moves = await getMoves();
  const payload = await getUploadPageData(moves);
  return json(payload);
};
