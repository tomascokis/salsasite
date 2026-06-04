import type { MoveRecord, PositionOption } from '$lib/types';
import { derivePositionOptions, positionSlug } from '$lib/video-library-utils';
import { getAppDatabase, recordAction, runInTransaction } from './app-state';

type PositionStore = {
  positions: Array<{ id?: string; label?: string; name?: string } | string>;
};

type StoredPositionRow = {
  id: string;
  label: string;
  sort_order: number;
};

function normalizePositionLabel(value: unknown) {
  return String(value ?? '').replace(/\s+/g, ' ').trim();
}

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

export async function savePositionOption(input: { id?: string; label?: string; name?: string }): Promise<PositionOption> {
  const label = normalizePositionLabel(input.label ?? input.name);
  if (!label) {
    throw new Error('Position label is required.');
  }

  const id = positionSlug(input.id || label);
  const timestamp = new Date().toISOString();

  return runInTransaction((db) => {
    const existing = db
      .prepare('SELECT id, label, sort_order FROM position_options WHERE id = ? LIMIT 1')
      .get(id) as StoredPositionRow | undefined;
    const sortOrder =
      existing?.sort_order ??
      Number((db.prepare('SELECT COALESCE(MAX(sort_order), -1) + 1 AS next FROM position_options').get() as { next: number }).next);
    const next: PositionOption = { id, label, source: 'custom' };

    db.prepare(
      `
        INSERT INTO position_options (id, label, sort_order)
        VALUES (?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
          label = excluded.label,
          sort_order = position_options.sort_order
      `
    ).run(id, label, sortOrder);

    recordAction(db, {
      type: existing ? 'position.update' : 'position.create',
      label: `${existing ? 'Updated' : 'Created'} position ${label}`,
      entityType: 'position',
      entityId: id,
      before: existing ? { id: existing.id, label: existing.label, source: 'custom' } : null,
      after: { ...next, updatedAt: timestamp }
    });

    return next;
  });
}
