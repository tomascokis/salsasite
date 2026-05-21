import { getLatestSnapshot, getLayout } from '$lib/server/data';

export async function load() {
  const [layout, snapshot] = await Promise.all([getLayout(), getLatestSnapshot()]);

  return {
    layout,
    snapshot
  };
}
