import { error } from '@sveltejs/kit';
import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';

export async function load({ params }) {
  const [moves, rawReferences] = await Promise.all([getMoves(), getRawMoveReference()]);
  const metadata = await getSiteMetadata(moves, rawReferences);
  const move = moves.find((entry) => entry.slug === params.slug) ?? null;

  if (!move) {
    throw error(404, 'Move not found');
  }

  return {
    move,
    metadata,
    moves
  };
}
