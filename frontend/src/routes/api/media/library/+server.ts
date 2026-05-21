import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import { getMediaLibraryPage } from '$lib/server/video-library';

export async function GET({ url }) {
  const moves = await getMoves();
  const limit = Number(url.searchParams.get('limit') ?? 50);
  const cursor = url.searchParams.get('cursor');

  return json(await getMediaLibraryPage(moves, { limit, cursor }));
}
