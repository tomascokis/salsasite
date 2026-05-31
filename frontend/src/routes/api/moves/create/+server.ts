import { json } from '@sveltejs/kit';
import { moveDisplayId } from '$lib/move-id';
import { draftMoveIdFromName } from '$lib/move-id-utils.js';
import { getMoves } from '$lib/server/data';
import { deleteMoveDraft, listMoveDrafts, publishMoveDraft, saveMoveDraft } from '$lib/server/move-editor';
import { relinkDerivedClipsForPublishedMove } from '$lib/server/video-library';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ request }) => {
  const body = await request.json();
  const action = String(body.action ?? 'saveDraft');

  try {
    if (action === 'publishDraft') {
      const moves = await getMoves();
      const draftId = String(body.draftId ?? '');
      const drafts = await listMoveDrafts();
      const draft = drafts.find((entry) => entry.draftId === draftId) ?? null;
      const move = await publishMoveDraft(moves, draftId, body.move);
      const previousMoveId = draft?.move.id ?? move.id;
      const previousDisplayId = draft ? moveDisplayId(draft.move) : previousMoveId;
      const nextDisplayId = moveDisplayId(move);

      if (previousMoveId !== move.id || previousDisplayId !== nextDisplayId) {
        await relinkDerivedClipsForPublishedMove(previousMoveId, move.id, nextDisplayId);
      }

      return json({ ok: true, move });
    }

    if (action === 'deleteDraft') {
      const draft = await deleteMoveDraft(String(body.draftId ?? ''));
      return json({ ok: true, draftId: draft.draftId });
    }

    if (action === 'createDraftFromName') {
      const name = String(body.name ?? '').trim();
      if (!name) {
        throw new Error('Move name is required.');
      }

      const [moves, drafts] = await Promise.all([getMoves(), listMoveDrafts()]);
      const id = draftMoveIdFromName(name, [
        ...moves.map((move) => move.id),
        ...drafts.map((draft) => draft.move.id)
      ]);
      const draft = await saveMoveDraft(moves, { id, name });
      return json({ ok: true, draft });
    }

    const moves = await getMoves();
    const draftId = body.draftId ? String(body.draftId) : undefined;
    const existingDraft = draftId
      ? (await listMoveDrafts()).find((entry) => entry.draftId === draftId) ?? null
      : null;
    const savedDraft = await saveMoveDraft(moves, {
      ...(body.move ?? {}),
      draftId
    });

    if (existingDraft) {
      const previousMoveId = existingDraft.move.id;
      const previousDisplayId = moveDisplayId(existingDraft.move);
      const nextMoveId = savedDraft.move.id;
      const nextDisplayId = moveDisplayId(savedDraft.move);

      if (previousMoveId !== nextMoveId || previousDisplayId !== nextDisplayId) {
        await relinkDerivedClipsForPublishedMove(previousMoveId, nextMoveId, nextDisplayId);
      }
    }

    return json({ ok: true, draft: savedDraft });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not save move draft.' },
      { status: 400 }
    );
  }
};
