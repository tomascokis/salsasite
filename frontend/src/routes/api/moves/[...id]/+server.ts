import { json } from '@sveltejs/kit';
import { moveDisplayId, normalizeMoveDisplayId } from '$lib/move-id';
import { runWithActionActor } from '$lib/server/app-state';
import { requireAdmin } from '$lib/server/auth-guard';
import { getMoves } from '$lib/server/data';
import { savePublishedMove } from '$lib/server/move-editor';
import { syncDerivedClipDisplayIdForMove } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async (event) => {
  const actor = requireAdmin(event).username;
  const { params, request } = event;
  const id = String(params.id ?? '').trim().toUpperCase();
  const body = await request.json();
  const moves = await getMoves();
  const existing = moves.find((move) => move.id === id) ?? null;

  try {
    const move = await runWithActionActor(actor, () => savePublishedMove(moves, id, body));
    const previousDisplayId = existing ? moveDisplayId(existing) : id;
    const nextDisplayId = normalizeMoveDisplayId(body.displayId) ?? moveDisplayId(move);
    if (nextDisplayId !== previousDisplayId) {
      await syncDerivedClipDisplayIdForMove(id, nextDisplayId);
    }
    return json({ ok: true, move });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not save move.' },
      { status: 400 }
    );
  }
};
