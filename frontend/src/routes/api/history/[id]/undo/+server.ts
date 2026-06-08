import { json } from '@sveltejs/kit';
import { runWithActionActor } from '$lib/server/app-state';
import { requireAdmin } from '$lib/server/auth-guard';
import { undoAction } from '$lib/server/history';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async (event) => {
  const actor = requireAdmin(event).username;
  const { params } = event;
  try {
    return json({
      ok: true,
      ...(await runWithActionActor(actor, () => undoAction(params.id)))
    });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not undo action.' },
      { status: 400 }
    );
  }
};
