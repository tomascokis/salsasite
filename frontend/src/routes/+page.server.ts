import { getLayout, getManifest, getSearchIndex } from '$lib/server/data';

export async function load() {
  const [manifest, layout, searchIndex] = await Promise.all([
    getManifest(),
    getLayout(),
    getSearchIndex()
  ]);

  return {
    manifest,
    layout,
    searchIndex
  };
}
