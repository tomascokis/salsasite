import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import { getMediaLibraryPage } from '$lib/server/video-library';

export async function GET({ url }) {
  const moves = await getMoves();
  const limit = Number(url.searchParams.get('limit') ?? 50);
  const cursor = url.searchParams.get('cursor');
  const publication = url.searchParams.get('publication') ?? 'all';
  const environment = url.searchParams.get('environment') ?? 'all';
  const dancers = url.searchParams.getAll('dancer');

  return json(
    await getMediaLibraryPage(moves, {
      limit,
      cursor,
      publication: publication === 'published' || publication === 'unpublished' || publication === 'draft' ? publication : 'all',
      environment: environment === 'class' || environment === 'social' ? environment : 'all',
      dancers
    })
  );
}
