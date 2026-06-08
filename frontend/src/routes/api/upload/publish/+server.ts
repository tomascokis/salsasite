import { json } from '@sveltejs/kit';
import { runWithActionActor } from '$lib/server/app-state';
import { requireAdmin } from '$lib/server/auth-guard';
import { publishClipsToMoves } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async (event) => {
  const actor = requireAdmin(event).username;
  const { request } = event;
  const body = await request.json();
  const clipIds: string[] = Array.isArray(body.clipIds)
    ? body.clipIds.map((value: unknown) => String(value).trim()).filter(Boolean)
    : [];

  try {
    const clips = await runWithActionActor(actor, () => publishClipsToMoves(clipIds));
    return json({ ok: true, clips });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not publish clips.' },
      { status: 400 }
    );
  }
};
