import type { MoveRecord } from '$lib/types';

export function normalizeMoveDisplayId(value: unknown) {
  const text = String(value ?? '').trim().toUpperCase();
  return text || null;
}

export function moveDisplayId(move: Pick<MoveRecord, 'id' | 'displayId'>) {
  return normalizeMoveDisplayId(move.displayId) ?? move.id;
}
