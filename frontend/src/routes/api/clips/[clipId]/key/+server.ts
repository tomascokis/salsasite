import { json } from '@sveltejs/kit';
import { runWithActionActor } from '$lib/server/app-state';
import { requireAdmin } from '$lib/server/auth-guard';
import { setClipKeyVideo } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async (event) => {
  const actor = requireAdmin(event).username;
  const { params, request } = event;
  const clipId = String(params.clipId ?? '').trim();
  const body = await request.json();
  const isKeyVideo = Boolean(body.isKeyVideo);

  if (!clipId) {
    return json({ error: 'clipId is required.' }, { status: 400 });
  }

  try {
    const clip = await runWithActionActor(actor, () => setClipKeyVideo({ clipId, isKeyVideo }));
    return json({ ok: true, clip });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not update key video state.' },
      { status: 400 }
    );
  }
};
