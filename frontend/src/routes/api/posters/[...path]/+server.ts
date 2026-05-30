import { json } from '@sveltejs/kit';
import {
  findPosterForVideoFile,
  isPosterGenerationPending,
  isPosterGeneratorUnavailable,
  queuePosterGeneration
} from '$lib/server/posters';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async ({ params }) => {
  const rawPath = params.path ?? '';
  const decodedPath = decodeURIComponent(rawPath);
  const videoFile = decodedPath;

  const posterFile = await findPosterForVideoFile(videoFile);
  if (posterFile) {
    return json({
      ready: true,
      pending: false,
      unavailable: false,
      posterFile
    });
  }

  if (isPosterGeneratorUnavailable()) {
    return json({
      ready: false,
      pending: false,
      unavailable: true,
      posterFile: null
    });
  }

  void queuePosterGeneration(videoFile);

  return json({
    ready: false,
    pending: isPosterGenerationPending(videoFile),
    unavailable: false,
    posterFile: null
  });
};
