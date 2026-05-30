import type { LayoutColumn, LayoutEntry, MoveRecord } from '$lib/types';
import { deriveOverviewTitle } from '$lib/server/overview.js';

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
      const filteredDataEntries = column.entries.filter((entry) => {
          if (entry.entryType !== 'Data') {
            return false;
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
        });

      const entries = preserveRelevantTitles(
        filteredDataEntries.map((entry) => {
          const move = entry.id ? moveById.get(entry.id.toLocaleLowerCase()) ?? null : null;
          return {
            entry,
            title: deriveOverviewTitle(entry, move)
          };
        })
      );

      return { ...column, entries };
    })
    .filter((column) => column.entries.some((entry) => entry.entryType === 'Data'));
}

function preserveRelevantTitles(entries: { entry: LayoutEntry; title: string }[]) {
  const result: LayoutEntry[] = [];
  let currentTitle = '';

  for (const { entry, title } of entries) {
    if (normalizeComparable(title) !== normalizeComparable(currentTitle)) {
      currentTitle = title;
      result.push({
        id: null,
        slug: null,
        name: title,
        entryType: 'Title',
        group: title,
        level: null,
        type: null,
        layoutOrder: entry.layoutOrder,
        levelOrder: null,
        valid: false
      });
    }

    result.push(entry);
  }

  return result;
}

function normalizeComparable(value: string | null | undefined) {
  return String(value ?? '')
    .toLocaleLowerCase()
    .replace(/\s+/g, ' ')
    .trim();
}
