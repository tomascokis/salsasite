import type { MoveRecord } from '$lib/types';
import { readMediaCatalog } from './media-catalog';
import { runMediaCatalogRepairs } from './media-repair-service';

export async function readVideoLibrary() {
  return readMediaCatalog();
}

export async function getVideoLibraryWithRepairs(moves?: MoveRecord[]) {
  if (moves) {
    await runMediaCatalogRepairs(moves, { audit: false });
  }

  return readMediaCatalog();
}

export const getVideoLibrary = getVideoLibraryWithRepairs;
