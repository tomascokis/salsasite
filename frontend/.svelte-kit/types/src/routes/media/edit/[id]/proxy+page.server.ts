// @ts-nocheck
import { error } from '@sveltejs/kit';
import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getDancerProfiles } from '$lib/server/dancers';
import { listMoveDrafts } from '$lib/server/move-editor';
import { getPositionOptions } from '$lib/server/positions';
import { getUploadPageData, getMediaLibraryPage, getVideoLibrary } from '$lib/server/video-library';
import type { VideoContentType, VideoEnvironment, VideoTiming } from '$lib/types';
import type { PageServerLoad } from './$types';

const timingOptions: Array<{ value: VideoTiming; label: string }> = [
  { value: 'on1', label: 'On1' },
  { value: 'on2', label: 'On2' },
  { value: 'other', label: 'Other' }
];

const contentTypeOptions: Array<{ value: VideoContentType; label: string }> = [
  { value: 'music', label: 'Music' },
  { value: 'counts', label: 'Counts' },
  { value: 'other', label: 'Other' }
];

const environmentOptions: Array<{ value: VideoEnvironment; label: string }> = [
  { value: 'social', label: 'Social' },
  { value: 'class', label: 'Class' }
];

export const load = async ({ params, url }: Parameters<PageServerLoad>[0]) => {
  const [moves, moveDrafts, rawReferences] = await Promise.all([getMoves(), listMoveDrafts(), getRawMoveReference()]);
  const uploadData = await getUploadPageData(moves);
  const selected = uploadData.assets.find((asset) => asset.id === params.id);

  if (!selected) {
    throw error(404, 'Source video not found');
  }

  const mediaData = await getMediaLibraryPage(moves, { limit: 1 });
  const [dancers, positionOptions] = await Promise.all([
    getDancerProfiles(moves, rawReferences, await getVideoLibrary(moves)),
    getPositionOptions(moves)
  ]);
  const publishedMoveIds = new Set(moves.map((move) => move.id));
  const draftMoveOptions = moveDrafts
    .map((draft) => draft.move)
    .filter((move) => !publishedMoveIds.has(move.id))
    .map((move) => ({
      id: move.id,
      slug: move.slug,
      name: move.name,
      isDraft: true
    }));

  return {
    timingOptions,
    contentTypeOptions,
    environmentOptions,
    dancerOptions: dancers.map((dancer) => dancer.displayName),
    positionOptions,
    moves: [
      ...moves.map((move) => ({
        id: move.id,
        slug: move.slug,
        name: move.name
      })),
      ...draftMoveOptions
    ],
    assets: [selected],
    total: 1,
    nextCursor: null,
    suggestions: mediaData.suggestions,
    selectedAssetId: selected.id,
    selectedClipId: url.searchParams.get('clip')
  };
};
