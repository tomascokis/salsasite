import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import { getUploadPageData } from '$lib/server/video-library';

export async function GET() {
  const moves = await getMoves();
  const payload = await getUploadPageData(moves);
  return json(payload);
}
