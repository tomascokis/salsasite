import { error } from '@sveltejs/kit';
import { getMoves, getOverviewLayout, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';
import { filterOverviewLayout } from '$lib/overview-filter';

export async function load({ params }) {
  const [moves, rawReferences, layout] = await Promise.all([getMoves(), getRawMoveReference(), getOverviewLayout()]);
  const metadata = await getSiteMetadata(moves, rawReferences);
  const topic = metadata.topics.find((entry) => entry.slug === params.slug);
  if (!topic) {
    throw error(404, 'Topic not found');
  }

  const filteredLayout = filterOverviewLayout(layout, moves, { topic: topic.name });

  return {
    entry: topic,
    layout: filteredLayout
  };
}
