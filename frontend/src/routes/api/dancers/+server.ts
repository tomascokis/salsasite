import { json } from '@sveltejs/kit';
import { deleteDancer, saveDancer } from '$lib/server/dancers';
import type { RequestHandler } from './$types';

export const POST: RequestHandler = async ({ request }) => {
  const payload = await request.json();

  try {
    const dancer = await saveDancer({
      id: payload.id,
      fullName: payload.fullName,
      displayName: payload.displayName,
      instagramHandle: payload.instagramHandle,
      role: payload.role,
      level: payload.level,
      region: payload.region
    });
    return json({ dancer });
  } catch (error) {
    return json({ error: error instanceof Error ? error.message : 'Could not save dancer.' }, { status: 400 });
  }
};

export const DELETE: RequestHandler = async ({ request }) => {
  try {
    const payload = await request.json();
    const dancer = await deleteDancer(payload.id);
    return json({ dancer });
  } catch (error) {
    return json({ error: error instanceof Error ? error.message : 'Could not delete dancer.' }, { status: 400 });
  }
};
