import fs from 'node:fs/promises';
import path from 'node:path';
import type {
  DancerLevel,
  DancerProfile,
  DancerRecord,
  DancerRole,
  MoveRecord,
  RawMoveReferenceRecord,
  VideoAsset,
  VideoLibrary
} from '$lib/types';
import { resolveDataDir } from './paths';
import { findPosterForVideoFile } from './posters';

type StoredDancer = DancerRecord;

type DancerStore = {
  version: 1;
  dancers: StoredDancer[];
};

type DancerInput = {
  id?: string;
  fullName?: string;
  displayName?: string;
  instagramHandle?: string | null;
  role?: DancerRole | null;
  level?: DancerLevel | null;
  region?: string | null;
};

const STORE_FILENAME = 'dancers.json';
const ROLE_VALUES = new Set<DancerRole>(['lead', 'follow', 'unknown']);
const LEVEL_VALUES = new Set<DancerLevel>(['world-class', 'pro', 'semi-pro', 'amateur', 'unknown']);

function emptyStore(): DancerStore {
  return {
    version: 1,
    dancers: []
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

function normalizeOptionalText(value: unknown) {
  const text = String(value ?? '').trim();
  return text || null;
}

function normalizeInstagramHandle(value: unknown) {
  const text = normalizeOptionalText(value);
  return text ? text.replace(/^@+/, '') : null;
}

function normalizeRole(value: unknown): DancerRole {
  return ROLE_VALUES.has(value as DancerRole) ? (value as DancerRole) : 'unknown';
}

function normalizeLevel(value: unknown): DancerLevel {
  return LEVEL_VALUES.has(value as DancerLevel) ? (value as DancerLevel) : 'unknown';
}

export function dancerSlug(value: string) {
  return normalizeName(value)
    .toLocaleLowerCase()
    .replace(/^@+/, '')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '') || 'unknown';
}

function dancerId(name: string) {
  return `dancer:${dancerSlug(name)}`;
}

function splitDancers(value: string | null | undefined) {
  return String(value ?? '')
    .split(/[,;/]+/)
    .map((entry) => normalizeName(entry))
    .filter(Boolean);
}

async function readStore() {
  try {
    const contents = await fs.readFile(storePath(), 'utf-8');
    const parsed = JSON.parse(contents) as Partial<DancerStore>;
    return {
      ...emptyStore(),
      ...parsed,
      dancers: Array.isArray(parsed.dancers) ? parsed.dancers : []
    };
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return emptyStore();
    }
    throw error;
  }
}

async function writeStore(store: DancerStore) {
  await fs.mkdir(resolveDataDir(), { recursive: true });
  await fs.writeFile(storePath(), `${JSON.stringify(store, null, 2)}\n`);
}

function derivedDancers(rawReferences: RawMoveReferenceRecord[], library: VideoLibrary) {
  const names = new Set<string>();
  for (const raw of rawReferences) {
    for (const dancer of splitDancers(raw.dancers)) {
      names.add(dancer);
    }
  }
  for (const asset of library.videoAssets) {
    for (const dancer of asset.dancers) {
      const name = normalizeName(dancer);
      if (name) names.add(name);
    }
  }

  const timestamp = nowIso();
  return [...names]
    .sort((left, right) => left.localeCompare(right, undefined, { sensitivity: 'base' }))
    .map((name): DancerRecord => ({
      id: dancerId(name),
      slug: dancerSlug(name),
      fullName: name,
      displayName: name.replace(/^@+/, ''),
      instagramHandle: name.startsWith('@') ? name.replace(/^@+/, '') : null,
      role: 'unknown',
      level: 'unknown',
      region: null,
      source: 'derived',
      createdAt: timestamp,
      updatedAt: timestamp
    }));
}

function mergeDancers(derived: DancerRecord[], stored: StoredDancer[]) {
  const bySlug = new Map(derived.map((dancer) => [dancer.slug, dancer]));

  for (const dancer of stored) {
    const fullName = normalizeName(dancer.fullName || dancer.displayName);
    if (!fullName) continue;
    const slug = dancerSlug(fullName);
    const existing = bySlug.get(slug);
    bySlug.set(slug, {
      ...(existing ?? {
        id: dancer.id || dancerId(fullName),
        slug,
        source: 'custom' as const,
        createdAt: dancer.createdAt || nowIso()
      }),
      id: dancer.id || existing?.id || dancerId(fullName),
      slug,
      fullName,
      displayName: normalizeName(dancer.displayName) || fullName,
      instagramHandle: normalizeInstagramHandle(dancer.instagramHandle),
      role: normalizeRole(dancer.role),
      level: normalizeLevel(dancer.level),
      region: normalizeOptionalText(dancer.region),
      source: existing ? existing.source : 'custom',
      createdAt: dancer.createdAt || existing?.createdAt || nowIso(),
      updatedAt: dancer.updatedAt || existing?.updatedAt || nowIso()
    });
  }

  return [...bySlug.values()].sort((left, right) => left.displayName.localeCompare(right.displayName, undefined, { sensitivity: 'base' }));
}

function assetHasDancer(asset: VideoAsset, dancer: DancerRecord) {
  const aliases = new Set([dancer.fullName, dancer.displayName, dancer.instagramHandle ? `@${dancer.instagramHandle}` : null, dancer.instagramHandle].filter(Boolean).map((value) => dancerSlug(String(value))));
  return asset.dancers.some((entry) => aliases.has(dancerSlug(entry)));
}

function rawHasDancer(raw: RawMoveReferenceRecord, dancer: DancerRecord) {
  const aliases = new Set([dancer.fullName, dancer.displayName, dancer.instagramHandle ? `@${dancer.instagramHandle}` : null, dancer.instagramHandle].filter(Boolean).map((value) => dancerSlug(String(value))));
  return splitDancers(raw.dancers).some((entry) => aliases.has(dancerSlug(entry)));
}

export async function getDancerProfiles(
  moves: MoveRecord[],
  rawReferences: RawMoveReferenceRecord[],
  library: VideoLibrary
): Promise<DancerProfile[]> {
  const store = await readStore();
  const dancers = mergeDancers(derivedDancers(rawReferences, library), store.dancers);
  const movesById = new Map(moves.map((move) => [move.id, move]));

  return Promise.all(
    dancers.map(async (dancer) => {
      const mediaAssets = library.videoAssets.filter((asset) => asset.kind === 'source' && assetHasDancer(asset, dancer));
      const rawMoveIds = new Set(rawReferences.filter((raw) => rawHasDancer(raw, dancer)).map((raw) => raw.id?.toUpperCase()).filter(Boolean) as string[]);
      const linkedMoveIds = new Set(
        library.moveVideoLinks
          .filter((link) => {
            const asset = library.videoAssets.find((candidate) => candidate.id === link.assetId);
            return asset ? assetHasDancer(asset, dancer) : false;
          })
          .map((link) => link.moveId)
      );

      const moveIds = new Set([...rawMoveIds, ...linkedMoveIds]);
      const dances = await Promise.all(
        mediaAssets.map(async (asset) => ({
          id: asset.id,
          displayName: asset.displayName,
          href: `/media/edit/${encodeURIComponent(asset.id)}`,
          posterFile: await findPosterForVideoFile(asset.filePath),
          meta: [asset.timing, asset.contentType, asset.environment].join(' · ')
        }))
      );

      const dancerMoves = [...moveIds]
        .map((id) => movesById.get(id) ?? null)
        .filter((move): move is MoveRecord => Boolean(move))
        .map((move) => ({
          id: move.id,
          slug: move.slug,
          name: move.name,
          topic: move.topic
        }));

      return {
        ...dancer,
        dances,
        moves: dancerMoves
      };
    })
  );
}

export async function saveDancer(input: DancerInput) {
  const fullName = normalizeName(input.fullName);
  if (!fullName) {
    throw new Error('Full name is required.');
  }

  const store = await readStore();
  const id = input.id || dancerId(fullName);
  const existing = store.dancers.find((dancer) => dancer.id === id) ?? store.dancers.find((dancer) => dancer.slug === dancerSlug(fullName));
  const timestamp = nowIso();
  const next: DancerRecord = {
    id: existing?.id ?? id,
    slug: dancerSlug(fullName),
    fullName,
    displayName: normalizeName(input.displayName) || fullName,
    instagramHandle: normalizeInstagramHandle(input.instagramHandle),
    role: normalizeRole(input.role),
    level: normalizeLevel(input.level),
    region: normalizeOptionalText(input.region),
    source: existing?.source ?? 'custom',
    createdAt: existing?.createdAt ?? timestamp,
    updatedAt: timestamp
  };

  store.dancers = existing
    ? store.dancers.map((dancer) => (dancer.id === existing.id ? next : dancer))
    : [...store.dancers, next];

  await writeStore(store);
  return next;
}
