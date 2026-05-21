import { json } from '@sveltejs/kit';
import { getSearchIndex } from '$lib/server/data';

export async function GET({ url }) {
  const q = url.searchParams.get('q')?.trim().toLowerCase() ?? '';
  const searchIndex = await getSearchIndex();

  if (!q) {
    return json({ results: [] });
  }

  const results = searchIndex
    .filter((entry) => `${entry.title ?? ''} ${entry.topic ?? ''} ${entry.text}`.toLowerCase().includes(q))
    .slice(0, 40);

  return json({ results });
}
