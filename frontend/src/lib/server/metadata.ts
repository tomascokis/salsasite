import type { MetadataEntry, MetadataKind, MoveRecord, RawMoveReferenceRecord, SiteMetadata } from '$lib/types';
import { getAppDatabase, recordAction, runInTransaction } from './app-state';

type StoredMetadataEntry = Omit<MetadataEntry, 'moveCount'>;

type MetadataInput = {
  id?: string;
  name?: string;
  description?: string | null;
};

function nowIso() {
  return new Date().toISOString();
}

function normalizeName(value: unknown) {
  return String(value ?? '').replace(/\s+/g, ' ').trim();
}

function normalizeDescription(value: unknown) {
  const text = String(value ?? '').trim();
  return text || null;
}

export function metadataSlug(value: string) {
  return normalizeName(value)
    .toLocaleLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '') || 'untitled';
}

function entryId(kind: MetadataKind, name: string) {
  return `${kind}:${metadataSlug(name)}`;
}

function metadataRowToEntry(row: Record<string, unknown>): StoredMetadataEntry {
  return {
    id: String(row.id),
    slug: String(row.slug),
    name: String(row.name),
    description: row.description == null ? null : String(row.description),
    createdAt: String(row.created_at),
    updatedAt: String(row.updated_at),
    source: row.source === 'derived' ? 'derived' : 'custom'
  };
}

function listStoredEntries(kind: MetadataKind) {
  return getAppDatabase()
    .prepare(
      `
        SELECT id, slug, name, description, created_at, updated_at, source
        FROM metadata_entries
        WHERE kind = ?
        ORDER BY name COLLATE NOCASE
      `
    )
    .all(kind)
    .map((row) => metadataRowToEntry(row as Record<string, unknown>));
}

function findStoredEntry(kind: MetadataKind, id: string, name: string) {
  const slug = metadataSlug(name);
  const row = getAppDatabase()
    .prepare(
      `
        SELECT id, slug, name, description, created_at, updated_at, source
        FROM metadata_entries
        WHERE kind = ? AND (id = ? OR slug = ?)
        LIMIT 1
      `
    )
    .get(kind, id, slug) as Record<string, unknown> | undefined;

  return row ? metadataRowToEntry(row) : null;
}

function upsertStoredEntry(db: ReturnType<typeof getAppDatabase>, kind: MetadataKind, entry: StoredMetadataEntry) {
  db.prepare(
    `
      INSERT INTO metadata_entries (
        id, kind, slug, name, description, created_at, updated_at, source
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(id) DO UPDATE SET
        kind = excluded.kind,
        slug = excluded.slug,
        name = excluded.name,
        description = excluded.description,
        created_at = excluded.created_at,
        updated_at = excluded.updated_at,
        source = excluded.source
    `
  ).run(
    entry.id,
    kind,
    entry.slug,
    entry.name,
    entry.description,
    entry.createdAt,
    entry.updatedAt,
    entry.source
  );
}

export function restoreMetadataEntryState(
  db: ReturnType<typeof getAppDatabase>,
  kind: MetadataKind,
  id: string,
  state: StoredMetadataEntry | null
) {
  db.prepare('DELETE FROM metadata_entries WHERE id = ? AND kind = ?').run(id, kind);
  if (state) {
    upsertStoredEntry(db, kind, state);
  }
}

function derivedEntries(kind: MetadataKind, names: string[], countByName: Map<string, number>) {
  const createdAt = nowIso();
  return Array.from(new Set(names.map(normalizeName).filter(Boolean)))
    .sort((left, right) => left.localeCompare(right, undefined, { sensitivity: 'base' }))
    .map((name): MetadataEntry => {
      const slug = metadataSlug(name);
      return {
        id: entryId(kind, name),
        slug,
        name,
        description: null,
        createdAt,
        updatedAt: createdAt,
        source: 'derived',
        moveCount: countByName.get(slug) ?? 0
      };
    });
}

function countValues(values: Array<string | null | undefined>) {
  const counts = new Map<string, number>();
  for (const value of values) {
    const name = normalizeName(value);
    if (!name) continue;
    const slug = metadataSlug(name);
    counts.set(slug, (counts.get(slug) ?? 0) + 1);
  }
  return counts;
}

function mergeEntries(kind: MetadataKind, derived: MetadataEntry[], stored: StoredMetadataEntry[]) {
  const bySlug = new Map(derived.map((entry) => [entry.slug, entry]));

  for (const entry of stored) {
    const name = normalizeName(entry.name);
    if (!name) continue;
    const slug = metadataSlug(name);
    const existing = bySlug.get(slug);
    bySlug.set(slug, {
      ...(existing ?? {
        id: entry.id || entryId(kind, name),
        slug,
        name,
        moveCount: 0
      }),
      id: entry.id || existing?.id || slug,
      slug,
      name,
      description: normalizeDescription(entry.description),
      createdAt: entry.createdAt || existing?.createdAt || nowIso(),
      updatedAt: entry.updatedAt || existing?.updatedAt || nowIso(),
      source: existing ? existing.source : 'custom'
    });
  }

  return [...bySlug.values()].sort((left, right) => left.name.localeCompare(right.name, undefined, { sensitivity: 'base' }));
}

export async function getSiteMetadata(moves: MoveRecord[], rawReferences: RawMoveReferenceRecord[] = []): Promise<SiteMetadata> {
  const topicCounts = countValues(moves.map((move) => move.topic));
  const familyNames = [
    ...moves.map((move) => move.group),
    ...rawReferences.map((entry) => entry.family)
  ];
  const familyCounts = countValues(familyNames);

  const topics = mergeEntries(
    'topic',
    derivedEntries('topic', moves.map((move) => move.topic ?? ''), topicCounts),
    listStoredEntries('topic')
  );
  const families = mergeEntries(
    'family',
    derivedEntries('family', familyNames.filter(Boolean) as string[], familyCounts),
    listStoredEntries('family')
  );

  return { topics, families };
}

export async function saveMetadataEntry(kind: MetadataKind, input: MetadataInput) {
  const name = normalizeName(input.name);
  if (!name) {
    throw new Error('Name is required.');
  }

  const id = input.id || entryId(kind, name);
  const existing = findStoredEntry(kind, id, name);
  const timestamp = nowIso();
  const next: StoredMetadataEntry = {
    id: existing?.id ?? id,
    slug: metadataSlug(name),
    name,
    description: normalizeDescription(input.description),
    createdAt: existing?.createdAt ?? timestamp,
    updatedAt: timestamp,
    source: existing?.source ?? 'custom'
  };

  runInTransaction((db) => {
    upsertStoredEntry(db, kind, next);
    recordAction(db, {
      type: existing ? 'metadata.update' : 'metadata.create',
      label: `${existing ? 'Updated' : 'Created'} ${kind} ${next.name}`,
      entityType: `metadata:${kind}`,
      entityId: next.id,
      before: existing,
      after: next
    });
  });

  return next;
}
