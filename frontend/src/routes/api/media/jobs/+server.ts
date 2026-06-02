import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import {
  exportMediaCatalogSnapshot,
  listMediaManagerJobs,
  queueSourceHashBackfill,
  runMediaCatalogRepairs,
  scanMediaCatalogRepairs
} from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const GET: RequestHandler = async ({ url }) => {
  const limit = Number(url.searchParams.get('limit') ?? 100);
  return json({
    jobs: listMediaManagerJobs(limit)
  });
};

export const POST: RequestHandler = async ({ request }) => {
  const payload = await request.json().catch(() => ({}));

  try {
    if (payload?.action === 'source.hash.backfill') {
      return json({
        ok: true,
        ...(await queueSourceHashBackfill())
      });
    }

    if (payload?.action === 'catalog.repair.scan') {
      return json({
        ok: true,
        repair: await scanMediaCatalogRepairs(await getMoves())
      });
    }

    if (payload?.action === 'catalog.repair.run') {
      return json({
        ok: true,
        repair: await runMediaCatalogRepairs(await getMoves())
      });
    }

    if (payload?.action === 'catalog.export.json') {
      const exported = await exportMediaCatalogSnapshot();
      return json({
        ok: true,
        export: {
          filePath: exported.filePath,
          counts: exported.counts
        }
      });
    }

    return json({ error: 'Unsupported media job action.' }, { status: 400 });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not complete media job action.' },
      { status: 400 }
    );
  }
};
