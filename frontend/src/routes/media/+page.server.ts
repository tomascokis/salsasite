import { getMoves } from '$lib/server/data';
import { getMediaLibraryPage } from '$lib/server/video-library';
import type { VideoContentType, VideoEnvironment, VideoTiming } from '$lib/types';

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

export async function load() {
  const moves = await getMoves();
  const mediaData = await getMediaLibraryPage(moves, { limit: 50 });

  return {
    timingOptions,
    contentTypeOptions,
    environmentOptions,
    moves: moves.map((move) => ({
      id: move.id,
      slug: move.slug,
      name: move.name
    })),
    ...mediaData
  };
}
