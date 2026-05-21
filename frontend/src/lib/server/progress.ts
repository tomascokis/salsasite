import type {
  LayoutColumn,
  LayoutEntry,
  ProgressSnapshot,
  ProgressView,
  ProgressViewColumn,
  ProgressViewEntry
} from '$lib/types';

function asViewEntry(layoutEntry: LayoutEntry, snapshot: ProgressSnapshot): ProgressViewEntry {
  if (layoutEntry.entryType === 'Title') {
    return {
      kind: 'title',
      name: layoutEntry.name ?? '',
      group: layoutEntry.group ?? null,
      layoutOrder: layoutEntry.layoutOrder ?? null
    };
  }

  const progress = snapshot.entries.find((entry) => entry.id === layoutEntry.id) ?? null;

  return {
    kind: 'move',
    id: layoutEntry.id ?? '',
    slug: layoutEntry.slug ?? '',
    name: layoutEntry.name ?? '',
    level: layoutEntry.level ?? null,
    group: layoutEntry.group ?? null,
    type: layoutEntry.type ?? null,
    prep: progress?.statNumPrep ?? null,
    sequ: progress?.statNumSequ ?? null,
    succ: progress?.statNumSucc ?? null,
    layoutOrder: layoutEntry.layoutOrder ?? null
  };
}

export function buildProgressView(layout: LayoutColumn[], snapshot: ProgressSnapshot): ProgressView {
  const columns: ProgressViewColumn[] = layout.map((column) => ({
    column: column.column,
    entries: column.entries.map((entry) => asViewEntry(entry, snapshot))
  }));

  return {
    date: snapshot.date,
    label: new Date(`${snapshot.date}T00:00:00`).toLocaleDateString('en-AU', {
      month: 'short',
      year: 'numeric'
    }),
    summary: snapshot.summary,
    columns
  };
}
