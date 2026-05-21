import fs from 'node:fs/promises';
import path from 'node:path';
import type { MetadataEntry, MetadataKind, MoveRecord, RawMoveReferenceRecord, SiteMetadata } from '$lib/types';
import { resolveDataDir } from './paths';

type StoredMetadataEntry = Omit<MetadataEntry, 'moveCount'>;

type MetadataStore = {
  version: 1;
  topics: StoredMetadataEntry[];
  families: StoredMetadataEntry[];
};

type MetadataInput = {
  id?: string;
  name?: string;
  description?: string | null;
};

const STORE_FILENAME = 'metadata.json';

function emptyStore(): MetadataStore {
  return {
    version: 1,
    topics: [],
    families: []
  };
}

function storePath() {
  return path.join(resolveDataDir(), STORE_FILENAME);
}

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

function collectionFor(store: MetadataStore, kind: MetadataKind) {
  return kind === 'topic' ? store.topics : store.families;
}

async function readStore() {
  try {
    const contents = await fs.readFile(storePath(), 'utf-8');
    const parsed = JSON.parse(contents) as Partial<MetadataStore>;
    return {
      ...emptyStore(),
      ...parsed,
      topics: Array.isArray(parsed.topics) ? parsed.topics : [],
      families: Array.isArray(parsed.families) ? parsed.families : []
    };
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return emptyStore();
    }
    throw error;
  }
}

async function writeStore(store: MetadataStore) {
  await fs.mkdir(resolveDataDir(), { recursive: true });
  await fs.writeFile(storePath(), `${JSON.stringify(store, null, 2)}\n`);
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
  const store = await readStore();
  const topicCounts = countValues(moves.map((move) => move.topic));
  const familyNames = [
    ...moves.map((move) => move.group),
    ...rawReferences.map((entry) => entry.family)
  ];
  const familyCounts = countValues(familyNames);

  const topics = mergeEntries(
    'topic',
    derivedEntries('topic', moves.map((move) => move.topic ?? ''), topicCounts),
    store.topics
  );
  const families = mergeEntries('family', derivedEntries('family', familyNames.filter(Boolean) as string[], familyCounts), store.families);

  return { topics, families };
}

export async function saveMetadataEntry(kind: MetadataKind, input: MetadataInput) {
  const name = normalizeName(input.name);
  if (!name) {
    throw new Error('Name is required.');
  }

  const store = await readStore();
  const collection = collectionFor(store, kind);
  const id = input.id || entryId(kind, name);
  const existing = collection.find((entry) => entry.id === id) ?? collection.find((entry) => metadataSlug(entry.name) === metadataSlug(name));
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

  const nextCollection = existing
    ? collection.map((entry) => (entry.id === existing.id ? next : entry))
    : [...collection, next];

  if (kind === 'topic') {
    store.topics = nextCollection;
  } else {
    store.families = nextCollection;
  }

  await writeStore(store);
  return next;
}
