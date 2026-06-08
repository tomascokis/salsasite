import { requireAdminPage } from '$lib/server/auth-guard';
import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getSiteMetadata } from '$lib/server/metadata';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async (event) => {
  requireAdminPage(event);
  const [moves, rawReferences] = await Promise.all([getMoves(), getRawMoveReference()]);
  const metadata = await getSiteMetadata(moves, rawReferences);

  return {
    metadata
  };
};
