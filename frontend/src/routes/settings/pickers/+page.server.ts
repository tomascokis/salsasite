import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getDancerProfiles } from '$lib/server/dancers';
import { getSiteMetadata } from '$lib/server/metadata';
import { findPosterForVideoFile } from '$lib/server/posters';
import { getVideoLibrary } from '$lib/server/video-library';

export async function load() {
  const [moves, rawReferences] = await Promise.all([getMoves(), getRawMoveReference()]);
  const [metadata, library] = await Promise.all([getSiteMetadata(moves, rawReferences), getVideoLibrary(moves)]);
  const dancers = await getDancerProfiles(moves, rawReferences, library);
  const moveOptions = await Promise.all(
    moves.slice(0, 120).map(async (move) => ({
      id: move.id,
      displayId: move.displayId ?? null,
      slug: move.slug,
      name: move.name,
      level: move.level,
      posterFile: move.videoFiles[0] ? await findPosterForVideoFile(move.videoFiles[0]) : null
    }))
  );

  return {
    families: metadata.families,
    dancers,
    moves: moveOptions
  };
}
