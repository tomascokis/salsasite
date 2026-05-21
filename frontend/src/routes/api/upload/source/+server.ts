import { json } from '@sveltejs/kit';
import { queuePosterGeneration } from '$lib/server/posters';
import { createSourceAsset } from '$lib/server/video-library';
import type { VideoContentType, VideoEnvironment, VideoOriginType, VideoTiming } from '$lib/types';

const VALID_TIMINGS = new Set<VideoTiming>(['on1', 'on2', 'other']);
const VALID_CONTENT_TYPES = new Set<VideoContentType>(['music', 'counts', 'other']);
const VALID_ENVIRONMENTS = new Set<VideoEnvironment>(['social', 'class']);
const VALID_ORIGIN_TYPES = new Set<VideoOriginType>(['self-recorded', 'download']);

export async function POST({ request }) {
  const formData = await request.formData();
  const file = formData.get('file');

  if (!(file instanceof File) || file.size === 0) {
    return json({ error: 'A source video file is required.' }, { status: 400 });
  }

  const timing = String(formData.get('timing') ?? '') as VideoTiming;
  const contentType = String(formData.get('contentType') ?? '') as VideoContentType;
  const environment = String(formData.get('environment') ?? '') as VideoEnvironment;
  const originType = String(formData.get('originType') ?? 'self-recorded') as VideoOriginType;

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

  const asset = await createSourceAsset({
    originalFilename: file.name,
    displayName: String(formData.get('displayName') ?? '').trim(),
    dancers: String(formData.get('dancers') ?? ''),
    timing,
    contentType,
    environment,
    originType,
    sourceUrl: String(formData.get('sourceUrl') ?? '').trim() || null,
    recordDate: String(formData.get('recordDate') ?? '').trim() || null,
    classWorkshop: String(formData.get('classWorkshop') ?? '').trim() || null,
    tags: String(formData.get('tags') ?? ''),
    notes: String(formData.get('notes') ?? '').trim() || null,
    fileBuffer: Buffer.from(await file.arrayBuffer())
  });

  void queuePosterGeneration(asset.filePath);

  return json({ ok: true, asset });
}
