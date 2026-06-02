// @ts-nocheck
import { error } from '@sveltejs/kit';
import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';
import { findPosterForVideoFile } from '$lib/server/posters';
import type { PageServerLoad } from './$types';

export const load = async ({ params }: Parameters<PageServerLoad>[0]) => {
  const [moves, rawReferences] = await Promise.all([getMoves(), getRawMoveReference()]);
  const metadata = await getSiteMetadata(moves, rawReferences);
  const move = moves.find((entry) => entry.slug === params.slug) ?? null;

  if (!move) {
    throw error(404, 'Move not found');
  }

  const previewFile = move.previewVideoFile ?? move.videoFiles[0] ?? null;

  return {
    move,
    metadata,
    moves,
    preview: previewFile
      ? {
          filePath: previewFile,
          posterFile: await findPosterForVideoFile(previewFile),
          label: move.name ?? move.displayId ?? move.id
        }
      : null
  };
};
