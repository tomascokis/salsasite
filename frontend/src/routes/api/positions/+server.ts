import { json } from '@sveltejs/kit';
import { savePositionOption } from '$lib/server/positions';
import type { RequestHandler } from '@sveltejs/kit';

export const POST: RequestHandler = async ({ request }) => {
  const payload = await request.json();

  try {
    const position = await savePositionOption({
      id: payload.id,
      label: payload.label ?? payload.name
    });
    return json({ position });
  } catch (error) {
    return json({ error: error instanceof Error ? error.message : 'Could not save position.' }, { status: 400 });
  }
};
