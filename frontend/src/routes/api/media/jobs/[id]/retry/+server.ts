import { json } from '@sveltejs/kit';
import { retryMediaManagerJob } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ params }) => {
  try {
    return json({
      ok: true,
      job: await retryMediaManagerJob(params.id)
    });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not retry media job.' },
      { status: 400 }
    );
  }
};
