import { json } from '@sveltejs/kit';
import { getRenderStatuses, queueClipRender } from '$lib/server/video-library';

export async function GET({ url }) {
  const ids = url.searchParams
    .get('ids')
    ?.split(',')
    .map((value) => value.trim())
    .filter(Boolean) ?? [];

  return json({
    statuses: await getRenderStatuses(ids)
  });
}

export async function POST({ request }) {
  const body = await request.json();
  const clipIds: string[] = Array.isArray(body.clipIds)
    ? body.clipIds.map((value: unknown) => String(value).trim()).filter(Boolean)
    : [];

  clipIds.forEach((clipId) => {
    void queueClipRender(clipId);
  });

  return json({
    ok: true,
    statuses: await getRenderStatuses(clipIds)
  });
}
