import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import { publishMoveDraft, saveMoveDraft } from '$lib/server/move-editor';

export async function POST({ request }) {
  const body = await request.json();
  const action = String(body.action ?? 'saveDraft');

  try {
    if (action === 'publishDraft') {
      const moves = await getMoves();
      const move = await publishMoveDraft(moves, String(body.draftId ?? ''), body.move);
      return json({ ok: true, move });
    }

    const draft = await saveMoveDraft({
      ...(body.move ?? {}),
      draftId: body.draftId ? String(body.draftId) : undefined
    });
    return json({ ok: true, draft });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not save move draft.' },
      { status: 400 }
    );
  }
}
