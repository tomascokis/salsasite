import { buildProgressView } from '$lib/server/progress';
import { getLayout, getProgressSnapshots } from '$lib/server/data';

export async function load() {
  const [layout, snapshots] = await Promise.all([getLayout(), getProgressSnapshots()]);

  return {
    views: snapshots.map((snapshot) => buildProgressView(layout, snapshot))
  };
}
