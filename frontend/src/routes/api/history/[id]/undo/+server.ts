import { json } from '@sveltejs/kit';
import { undoAction } from '$lib/server/history';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ params }) => {
  try {
    return json({
      ok: true,
      ...undoAction(params.id)
    });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not undo action.' },
      { status: 400 }
    );
  }
};

