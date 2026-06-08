import { json } from '@sveltejs/kit';
import { runWithActionActor } from '$lib/server/app-state';
import { requireAdmin } from '$lib/server/auth-guard';
import { savePositionOption } from '$lib/server/positions';
import type { RequestHandler } from '@sveltejs/kit';

export const POST: RequestHandler = async (event) => {
  const actor = requireAdmin(event).username;
  const { request } = event;
  const payload = await request.json();

  try {
    const position = await runWithActionActor(actor, () => savePositionOption({
      id: payload.id,
      label: payload.label ?? payload.name
    }));
    return json({ position });
  } catch (error) {
    return json({ error: error instanceof Error ? error.message : 'Could not save position.' }, { status: 400 });
  }
};
