import type { MoveRecord, PositionOption } from '$lib/types';
import { derivePositionOptions } from '$lib/video-library-utils';
import { getAppDatabase } from './app-state';

type PositionStore = {
  positions: Array<{ id?: string; label?: string; name?: string } | string>;
};

function readStore(): PositionStore {
  const rows = getAppDatabase()
    .prepare('SELECT id, label FROM position_options ORDER BY sort_order, label')
    .all() as Array<{ id: string; label: string }>;
  return {
    positions: rows.map((row) => ({ id: row.id, label: row.label }))
  };
}

export async function getPositionOptions(moves: MoveRecord[]): Promise<PositionOption[]> {
  const store = readStore();
  return derivePositionOptions(moves, store.positions) as PositionOption[];
}

export function positionLabelById(options: PositionOption[]) {
  return new Map(options.map((option) => [option.id, option.label]));
}
