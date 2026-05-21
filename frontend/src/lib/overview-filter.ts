import type { LayoutColumn, LayoutEntry, MoveRecord } from '$lib/types';

export interface OverviewFilter {
  topic?: string | null;
  family?: string | null;
  moveIds?: string[];
  predicate?: (entry: LayoutEntry, move: MoveRecord | null) => boolean;
}

export function filterOverviewLayout(
  layout: LayoutColumn[],
  moves: MoveRecord[],
  filter: OverviewFilter
): LayoutColumn[] {
  const moveById = new Map(moves.map((move) => [move.id.toLocaleLowerCase(), move]));
  const allowedMoveIds = filter.moveIds ? new Set(filter.moveIds.map((id) => id.toLocaleLowerCase())) : null;

  return layout
    .map((column) => {
      const entries = preserveRelevantTitles(
        column.entries.filter((entry) => {
          if (entry.entryType !== 'Data') {
            return true;
          }

          const move = entry.id ? moveById.get(entry.id.toLocaleLowerCase()) ?? null : null;
          if (allowedMoveIds && (!entry.id || !allowedMoveIds.has(entry.id.toLocaleLowerCase()))) {
            return false;
          }

          if (filter.topic && normalizeComparable(move?.topic) !== normalizeComparable(filter.topic)) {
            return false;
          }

          if (filter.family && normalizeComparable(move?.group) !== normalizeComparable(filter.family)) {
            return false;
          }

          return filter.predicate ? filter.predicate(entry, move) : true;
        })
      );

      return { ...column, entries };
    })
    .filter((column) => column.entries.some((entry) => entry.entryType === 'Data'));
}

function preserveRelevantTitles(entries: LayoutEntry[]) {
  const result: LayoutEntry[] = [];
  let pendingTitles: LayoutEntry[] = [];

  for (const entry of entries) {
    if (entry.entryType !== 'Data') {
      pendingTitles = [...pendingTitles, entry];
      continue;
    }

    result.push(...pendingTitles, entry);
    pendingTitles = [];
  }

  return result;
}

function normalizeComparable(value: string | null | undefined) {
  return String(value ?? '')
    .toLocaleLowerCase()
    .replace(/\s+/g, ' ')
    .trim();
}
