// @ts-nocheck
import { listHistory } from '$lib/server/history';
import type { PageServerLoad } from './$types';

export const load = async () => ({
  entries: listHistory(100)
});

;null as any as PageServerLoad;