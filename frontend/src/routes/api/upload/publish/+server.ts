import { json } from '@sveltejs/kit';
import { publishClipsToMoves } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ request }) => {
  const body = await request.json();
  const clipIds: string[] = Array.isArray(body.clipIds)
    ? body.clipIds.map((value: unknown) => String(value).trim()).filter(Boolean)
    : [];

  try {
    const clips = await publishClipsToMoves(clipIds);
    return json({ ok: true, clips });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not publish clips.' },
      { status: 400 }
    );
  }
};
