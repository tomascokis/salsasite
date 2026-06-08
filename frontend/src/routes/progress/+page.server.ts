import { buildProgressView } from '$lib/server/progress';
import { getOverviewLayout, getProgressSnapshots } from '$lib/server/data';

export async function load() {
  const [layout, snapshots] = await Promise.all([getOverviewLayout(), getProgressSnapshots()]);

  return {
    views: snapshots.map((snapshot) => buildProgressView(layout, snapshot))
  };
}
