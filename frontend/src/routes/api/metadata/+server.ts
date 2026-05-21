import { json } from '@sveltejs/kit';
import { saveMetadataEntry } from '$lib/server/metadata';
import type { MetadataKind } from '$lib/types';

function isMetadataKind(value: unknown): value is MetadataKind {
  return value === 'topic' || value === 'family';
}

export async function POST({ request }) {
  const payload = await request.json();
  if (!isMetadataKind(payload.kind)) {
    return json({ error: 'Metadata kind must be topic or family.' }, { status: 400 });
  }

  try {
    const entry = await saveMetadataEntry(payload.kind, {
      id: payload.id,
      name: payload.name,
      description: payload.description
    });
    return json({ entry });
  } catch (error) {
    return json({ error: error instanceof Error ? error.message : 'Could not save metadata.' }, { status: 400 });
  }
}
