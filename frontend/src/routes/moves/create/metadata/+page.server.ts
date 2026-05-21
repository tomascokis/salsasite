import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';

export async function load() {
  const [moves, rawReferences] = await Promise.all([getMoves(), getRawMoveReference()]);
  const metadata = await getSiteMetadata(moves, rawReferences);

  return {
    metadata
  };
}
