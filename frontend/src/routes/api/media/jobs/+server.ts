import { json } from '@sveltejs/kit';
import { listMediaManagerJobs, queueSourceHashBackfill } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async ({ url }) => {
  const limit = Number(url.searchParams.get('limit') ?? 100);
  return json({
    jobs: listMediaManagerJobs(limit)
  });
};

export const POST: RequestHandler = async ({ request }) => {
  const payload = await request.json().catch(() => ({}));
  if (payload?.action !== 'source.hash.backfill') {
    return json({ error: 'Unsupported media job action.' }, { status: 400 });
  }

  try {
    return json({
      ok: true,
      ...(await queueSourceHashBackfill())
    });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not queue source hash backfill.' },
      { status: 400 }
    );
  }
};
