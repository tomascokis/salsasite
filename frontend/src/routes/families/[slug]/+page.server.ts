import { error } from '@sveltejs/kit';
import { getMoves, getOverviewLayout, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata, metadataSlug } from '$lib/server/metadata';
import { filterOverviewLayout } from '$lib/overview-filter';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async ({ params }) => {
  const [moves, rawReferences, layout] = await Promise.all([getMoves(), getRawMoveReference(), getOverviewLayout()]);
  const metadata = await getSiteMetadata(moves, rawReferences);
  const family = metadata.families.find((entry) => entry.slug === params.slug);
  if (!family) {
    throw error(404, 'Family not found');
  }

  const moveIds = new Set<string>();
  for (const raw of rawReferences) {
    if (metadataSlug(raw.family ?? '') === family.slug && raw.id) {
      moveIds.add(raw.id.toUpperCase());
    }
  }
  for (const move of moves) {
    if (metadataSlug(move.group ?? '') === family.slug) {
      moveIds.add(move.id);
    }
  }

  const filteredLayout = filterOverviewLayout(layout, moves, { moveIds: [...moveIds] });

  return {
    entry: family,
    layout: filteredLayout
  };
};
