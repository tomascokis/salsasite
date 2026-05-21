import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import { savePublishedMove } from '$lib/server/move-editor';

export async function POST({ params, request }) {
  const id = String(params.id ?? '').trim().toUpperCase();
  const body = await request.json();
  const moves = await getMoves();

  try {
    const move = await savePublishedMove(moves, id, body);
    return json({ ok: true, move });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not save move.' },
      { status: 400 }
    );
  }
}
