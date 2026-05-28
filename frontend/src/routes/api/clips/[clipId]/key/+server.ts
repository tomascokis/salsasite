import { json } from '@sveltejs/kit';
import { setClipKeyVideo } from '$lib/server/video-library';

export async function POST({ params, request }) {
  const clipId = String(params.clipId ?? '').trim();
  const body = await request.json();
  const isKeyVideo = Boolean(body.isKeyVideo);

  if (!clipId) {
    return json({ error: 'clipId is required.' }, { status: 400 });
  }

  try {
    const clip = await setClipKeyVideo({ clipId, isKeyVideo });
    return json({ ok: true, clip });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not update key video state.' },
      { status: 400 }
    );
  }
}
