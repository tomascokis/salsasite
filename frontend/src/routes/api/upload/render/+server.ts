import { json } from '@sveltejs/kit';
import { runWithActionActor } from '$lib/server/app-state';
import { requireAdmin } from '$lib/server/auth-guard';
import { getRenderStatuses, queueClipRender } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async (event) => {
  requireAdmin(event);
  const { url } = event;
  const ids = url.searchParams
    .get('ids')
    ?.split(',')
    .map((value) => value.trim())
    .filter(Boolean) ?? [];

  return json({
    statuses: await getRenderStatuses(ids)
  });
};

export const POST: RequestHandler = async (event) => {
  const actor = requireAdmin(event).username;
  const { request } = event;
  const body = await request.json();
  const clipIds: string[] = Array.isArray(body.clipIds)
    ? body.clipIds.map((value: unknown) => String(value).trim()).filter(Boolean)
    : [];

  await runWithActionActor(actor, async () => clipIds.forEach((clipId) => {
    void queueClipRender(clipId);
  }));

  return json({
    ok: true,
    statuses: await getRenderStatuses(clipIds)
  });
};
