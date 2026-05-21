import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';
import { listCreatedMoveIds, listMoveDrafts } from '$lib/server/move-editor';
import { findPosterForVideoFile } from '$lib/server/posters';

export async function load() {
  const [moves, drafts, recentMoveIds, rawReferences] = await Promise.all([
    getMoves(),
    listMoveDrafts(),
    listCreatedMoveIds(),
    getRawMoveReference()
  ]);
  const metadata = await getSiteMetadata(moves, rawReferences);
  const moveCards = await Promise.all(
    moves.map(async (move) => ({
      ...move,
      posterFile: move.videoFiles[0] ? await findPosterForVideoFile(move.videoFiles[0]) : null
    }))
  );

  return {
    drafts,
    metadata,
    moves: moveCards,
    recentMoveIds
  };
}
