import { listHistory } from '$lib/server/history';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async () => ({
  entries: listHistory(100)
});

