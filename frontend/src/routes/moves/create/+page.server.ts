import { requireAdminPage } from '$lib/server/auth-guard';
import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';
import { listCreatedMoveIds, listMoveDrafts } from '$lib/server/move-editor';
import { findPosterForVideoFile } from '$lib/server/posters';
import { getResolvedMoveVideos } from '$lib/server/video-library';
import type { PageServerLoad } from './$types';

type MovePreview = {
  filePath: string;
  posterFile: string | null;
  label: string;
};

async function previewForFile(filePath: string | null | undefined, label: string): Promise<MovePreview | null> {
  if (!filePath) return null;
  return {
    filePath,
    posterFile: await findPosterForVideoFile(filePath),
    label
  };
}

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
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
  const draftPreviewEntries = await Promise.all(
    drafts.map(async (draft) => {
      const videos = await getResolvedMoveVideos(draft.move.id, moves);
      const video = videos[0] ?? null;
      const preview = await previewForFile(
        video?.lowResFilePath ?? video?.filePath ?? draft.move.previewVideoFile ?? draft.move.videoFiles[0],
        draft.move.name ?? draft.move.displayId ?? draft.move.id
      );
      return [
        [draft.draftId, preview],
        [draft.move.id, preview]
      ] as const;
    })
  );

  return {
    drafts,
    metadata,
    moves: moveCards,
    draftPreviews: Object.fromEntries(
      draftPreviewEntries.flat().filter((entry): entry is readonly [string, MovePreview] => Boolean(entry[1]))
    ),
    recentMoveIds
  };
};
