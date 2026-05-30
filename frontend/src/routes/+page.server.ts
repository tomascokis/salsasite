import { getManifest, getOverviewLayout, getSearchIndex } from '$lib/server/data';

export async function load() {
  const [manifest, layout, searchIndex] = await Promise.all([
    getManifest(),
    getOverviewLayout(),
    getSearchIndex()
  ]);

  return {
    manifest,
    layout,
    searchIndex
  };
}
