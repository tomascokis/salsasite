import { error } from '@sveltejs/kit';
import { getMoves, getRawMoveReference } from '$lib/server/data';
import { buildRelationshipDiagram } from '$lib/relationship-diagram';
import { getResolvedMoveVideos } from '$lib/server/video-library';
import { queuePosterGeneration } from '$lib/server/posters';
import { getSiteMetadata } from '$lib/server/metadata';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async ({ params }) => {
  const moves = await getMoves();
  const move = moves.find((entry) => entry.slug === params.slug) ?? null;

  if (!move) {
    throw error(404, 'Move not found');
  }

  const rawMoves = await getRawMoveReference();
  const rawReference = rawMoves.find((entry) => entry.id === move.id) ?? null;
  const relationshipDiagram = buildRelationshipDiagram(moves, move.id);
  const metadata = await getSiteMetadata(moves, rawMoves);
  const videos = await getResolvedMoveVideos(move.id, moves);

  videos.forEach((video) => {
    if (!video.posterFile) {
      void queuePosterGeneration(video.filePath);
    }
  });

  move.videoFiles.forEach((videoFile) => {
    if (!videos.some((video) => video.filePath === videoFile)) {
      void queuePosterGeneration(videoFile);
    }
  });

  return {
    move,
    rawReference,
    relationshipDiagram,
    metadata,
    moves: moves.map((entry) => ({
      id: entry.id,
      slug: entry.slug,
      name: entry.name,
      positions: entry.positions,
      tags: entry.tags,
      source: entry.source
    })),
    videos
  };
};
