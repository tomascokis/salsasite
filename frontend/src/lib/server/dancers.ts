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
import { getAppDatabase, recordAction, runInTransaction } from './app-state';
import { findPosterForVideoFile } from './posters';

type StoredDancer = DancerRecord;

type DancerInput = {
  id?: string;
  fullName?: string;
  displayName?: string;
  instagramHandle?: string | null;
  role?: DancerRole | null;
  level?: DancerLevel | null;
  region?: string | null;
};

const ROLE_VALUES = new Set<DancerRole>(['lead', 'follow', 'unknown']);
const LEVEL_VALUES = new Set<DancerLevel>(['world-class', 'pro', 'semi-pro', 'amateur', 'unknown']);

type DancerActionState = {
  dancer: StoredDancer | null;
  id: string;
  slug: string;
  deletedSlug: boolean;
};

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

function normalizeDeletedDancerSlugs(value: unknown) {
  if (!Array.isArray(value)) return [];
  return Array.from(
    new Set(
      value
        .map((entry) => normalizeName(entry))
        .filter(Boolean)
        .map((entry) => dancerSlug(entry))
        .filter(Boolean)
    )
  ).sort((left, right) => left.localeCompare(right));
}

function dancerSlugFromId(id: string) {
  return dancerSlug(id.startsWith('dancer:') ? id.slice('dancer:'.length) : id);
}

function dancerRowToRecord(row: Record<string, unknown>): StoredDancer {
  return {
    id: String(row.id),
    slug: String(row.slug),
    fullName: String(row.full_name),
    displayName: String(row.display_name),
    instagramHandle: row.instagram_handle == null ? null : String(row.instagram_handle),
    role: normalizeRole(row.role),
    level: normalizeLevel(row.level),
    region: normalizeOptionalText(row.region),
    source: row.source === 'derived' ? 'derived' : 'custom',
    createdAt: String(row.created_at),
    updatedAt: String(row.updated_at)
  };
}

function listStoredDancers() {
  return getAppDatabase()
    .prepare(
      `
        SELECT
          id, slug, full_name, display_name, instagram_handle, role, level,
          region, source, created_at, updated_at
        FROM dancer_profiles
        ORDER BY display_name COLLATE NOCASE
      `
    )
    .all()
    .map((row) => dancerRowToRecord(row as Record<string, unknown>));
}

function listDeletedDancerSlugs() {
  return normalizeDeletedDancerSlugs(
    getAppDatabase()
      .prepare('SELECT slug FROM deleted_dancer_slugs ORDER BY slug')
      .all()
      .map((row) => (row as { slug: string }).slug)
  );
}

function findStoredDancerByIdOrSlug(id: string, slug: string) {
  const row = getAppDatabase()
    .prepare(
      `
        SELECT
          id, slug, full_name, display_name, instagram_handle, role, level,
          region, source, created_at, updated_at
        FROM dancer_profiles
        WHERE id = ? OR slug = ?
        LIMIT 1
      `
    )
    .get(id, slug) as Record<string, unknown> | undefined;

  return row ? dancerRowToRecord(row) : null;
}

function upsertDancer(db: ReturnType<typeof getAppDatabase>, dancer: StoredDancer) {
  db.prepare('DELETE FROM dancer_profiles WHERE id = ? OR slug = ?').run(dancer.id, dancer.slug);
  db.prepare(
    `
      INSERT INTO dancer_profiles (
        id, slug, full_name, display_name, instagram_handle, role, level,
        region, source, created_at, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `
  ).run(
    dancer.id,
    dancer.slug,
    dancer.fullName,
    dancer.displayName,
    dancer.instagramHandle,
    dancer.role,
    dancer.level,
    dancer.region,
    dancer.source,
    dancer.createdAt,
    dancer.updatedAt
  );
}

function captureDancerState(id: string, slug: string): DancerActionState {
  return {
    dancer: findStoredDancerByIdOrSlug(id, slug),
    id,
    slug,
    deletedSlug: listDeletedDancerSlugs().includes(slug)
  };
}

export function restoreDancerState(db: ReturnType<typeof getAppDatabase>, state: DancerActionState) {
  if (state.dancer) {
    upsertDancer(db, state.dancer);
  } else {
    db.prepare('DELETE FROM dancer_profiles WHERE id = ? OR slug = ?').run(state.id, state.slug);
  }

  if (state.deletedSlug) {
    db.prepare('INSERT OR IGNORE INTO deleted_dancer_slugs (slug) VALUES (?)').run(state.slug);
  } else {
    db.prepare('DELETE FROM deleted_dancer_slugs WHERE slug = ?').run(state.slug);
  }
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

function mergeDancers(derived: DancerRecord[], stored: StoredDancer[], deletedDancerSlugs: Set<string>) {
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

  return [...bySlug.values()]
    .filter((dancer) => !deletedDancerSlugs.has(dancer.slug))
    .sort((left, right) => left.displayName.localeCompare(right.displayName, undefined, { sensitivity: 'base' }));
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
  const deletedDancerSlugs = new Set(listDeletedDancerSlugs());
  const dancers = mergeDancers(derivedDancers(rawReferences, library), listStoredDancers(), deletedDancerSlugs);
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

  const id = input.id || dancerId(fullName);
  const slug = dancerSlug(fullName);
  const existing = findStoredDancerByIdOrSlug(id, slug);
  const timestamp = nowIso();
  const next: DancerRecord = {
    id: existing?.id ?? id,
    slug,
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

  const before = captureDancerState(next.id, existing?.slug ?? next.slug);
  const after: DancerActionState = {
    dancer: next,
    id: next.id,
    slug: next.slug,
    deletedSlug: false
  };

  runInTransaction((db) => {
    upsertDancer(db, next);
    db.prepare('DELETE FROM deleted_dancer_slugs WHERE slug = ?').run(next.slug);
    recordAction(db, {
      type: existing ? 'dancer.update' : 'dancer.create',
      label: `${existing ? 'Updated' : 'Created'} dancer ${next.displayName}`,
      entityType: 'dancer',
      entityId: next.id,
      before,
      after
    });
  });

  return next;
}

export async function deleteDancer(id: string) {
  const normalizedId = String(id ?? '').trim();
  if (!normalizedId) {
    throw new Error('Dancer id is required.');
  }

  const existing = findStoredDancerByIdOrSlug(normalizedId, dancerSlugFromId(normalizedId));
  const slug = existing?.slug ?? dancerSlugFromId(normalizedId);
  const before = captureDancerState(normalizedId, slug);
  const after: DancerActionState = {
    dancer: null,
    id: normalizedId,
    slug,
    deletedSlug: true
  };

  runInTransaction((db) => {
    db.prepare('DELETE FROM dancer_profiles WHERE id = ? OR slug = ?').run(normalizedId, slug);
    db.prepare('INSERT OR IGNORE INTO deleted_dancer_slugs (slug) VALUES (?)').run(slug);
    recordAction(db, {
      type: 'dancer.delete',
      label: `Deleted dancer ${existing?.displayName ?? slug}`,
      entityType: 'dancer',
      entityId: normalizedId,
      before,
      after
    });
  });

  return {
    id: normalizedId,
    slug
  };
}
