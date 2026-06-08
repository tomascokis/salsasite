import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth-guard';
import { detectSourceAssetFields } from '$lib/server/video-library';
import type { VideoOriginType } from '$lib/types';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async (event) => {
  requireAdmin(event);
  const { params, request } = event;
  const body = await request.json().catch(() => ({}));
  const originType = String(body.originType ?? '');

  try {
    const result = await detectSourceAssetFields(params.id, {
      originType: originType ? (originType as VideoOriginType) : undefined
    });

    return json({ ok: true, ...result });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not detect source fields.' },
      { status: 404 }
    );
  }
};
