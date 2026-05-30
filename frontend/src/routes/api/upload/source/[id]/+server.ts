import { json } from '@sveltejs/kit';
import { deleteSourceAsset, updateSourceAsset } from '$lib/server/video-library';
import type { VideoContentType, VideoEnvironment, VideoOriginType, VideoTiming } from '$lib/types';
import type { RequestHandler } from './$types';

const VALID_TIMINGS = new Set<VideoTiming>(['on1', 'on2', 'other']);
const VALID_CONTENT_TYPES = new Set<VideoContentType>(['music', 'counts', 'other']);
const VALID_ENVIRONMENTS = new Set<VideoEnvironment>(['social', 'class']);
const VALID_ORIGIN_TYPES = new Set<VideoOriginType>(['self-recorded', 'download']);

export const PUT: RequestHandler = async ({ params, request }) => {
  const body = await request.json();
  const timing = String(body.timing ?? '') as VideoTiming;
  const contentType = String(body.contentType ?? '') as VideoContentType;
  const environment = String(body.environment ?? '') as VideoEnvironment;
  const originType = String(body.originType ?? 'self-recorded') as VideoOriginType;

  if (!VALID_TIMINGS.has(timing)) {
    return json({ error: 'Timing is required.' }, { status: 400 });
  }

  if (!VALID_CONTENT_TYPES.has(contentType)) {
    return json({ error: 'Type is required.' }, { status: 400 });
  }

  if (!VALID_ENVIRONMENTS.has(environment)) {
    return json({ error: 'Environment is required.' }, { status: 400 });
  }

  if (!VALID_ORIGIN_TYPES.has(originType)) {
    return json({ error: 'Source is required.' }, { status: 400 });
  }

  try {
    const asset = await updateSourceAsset({
      assetId: params.id,
      displayName: String(body.displayName ?? '').trim(),
      dancers: Array.isArray(body.dancers) ? body.dancers.map(String) : String(body.dancers ?? ''),
      timing,
      contentType,
      environment,
      originType,
      sourceUrl: String(body.sourceUrl ?? '').trim() || null,
      createdAt: String(body.createdAt ?? '').trim() || null,
      recordDate: String(body.recordDate ?? '').trim() || null,
      classWorkshop: String(body.classWorkshop ?? '').trim() || null,
      tags: Array.isArray(body.tags) ? body.tags.map(String) : String(body.tags ?? ''),
      notes: String(body.notes ?? '').trim() || null
    });

    return json({ ok: true, asset });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not update source asset.' },
      { status: 404 }
    );
  }
};

export const DELETE: RequestHandler = async ({ params }) => {
  try {
    const result = await deleteSourceAsset(params.id);
    return json({ ok: true, ...result });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not delete source asset.' },
      { status: 404 }
    );
  }
};
