import fs from 'node:fs';
import path from 'node:path';
import { randomUUID } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { resolveDataDir } from './paths';

export type ActionRecord = {
  id: string;
  type: string;
  label: string;
  entityType: string;
  entityId: string;
  status: 'active' | 'undone';
  createdAt: string;
  actor: string | null;
  before: unknown;
  after: unknown;
  undoneByActionId: string | null;
  undoOfActionId: string | null;
};

export type ActionInput = {
  type: string;
  label: string;
  entityType: string;
  entityId: string;
  before: unknown;
  after: unknown;
  actor?: string | null;
  undoOfActionId?: string | null;
};

type DatabaseRow = Record<string, unknown>;

const DB_FILENAME = 'app-state.sqlite';
let database: DatabaseSync | null = null;
let initialized = false;

function nowIso() {
  return new Date().toISOString();
}

function dbPath() {
  return path.join(resolveDataDir(), DB_FILENAME);
}

function readJsonFile<T>(filename: string, fallback: T): T {
  try {
    const contents = fs.readFileSync(path.join(resolveDataDir(), filename), 'utf-8');
    return JSON.parse(contents) as T;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return fallback;
    }
    throw error;
  }
}

function serializeJson(value: unknown) {
  return JSON.stringify(value ?? null);
}

function parseJson(value: unknown) {
  if (typeof value !== 'string' || !value) {
    return null;
  }
  return JSON.parse(value);
}

function statementRows<T>(statement: ReturnType<DatabaseSync['prepare']>, ...params: any[]): T[] {
  return statement.all(...params) as T[];
}

function createSchema(db: DatabaseSync) {
  db.exec(`
    CREATE TABLE IF NOT EXISTS app_state_meta (
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS actions (
      id TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      label TEXT NOT NULL,
      entity_type TEXT NOT NULL,
      entity_id TEXT NOT NULL,
      status TEXT NOT NULL,
      created_at TEXT NOT NULL,
      actor TEXT,
      before_json TEXT NOT NULL,
      after_json TEXT NOT NULL,
      undone_by_action_id TEXT,
      undo_of_action_id TEXT
    );

    CREATE INDEX IF NOT EXISTS idx_actions_created_at ON actions(created_at DESC);
    CREATE INDEX IF NOT EXISTS idx_actions_entity ON actions(entity_type, entity_id);

    CREATE TABLE IF NOT EXISTS metadata_entries (
      id TEXT PRIMARY KEY,
      kind TEXT NOT NULL,
      slug TEXT NOT NULL,
      name TEXT NOT NULL,
      description TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      source TEXT NOT NULL,
      UNIQUE(kind, slug)
    );

    CREATE TABLE IF NOT EXISTS dancer_profiles (
      id TEXT PRIMARY KEY,
      slug TEXT NOT NULL UNIQUE,
      full_name TEXT NOT NULL,
      display_name TEXT NOT NULL,
      instagram_handle TEXT,
      role TEXT NOT NULL,
      level TEXT NOT NULL,
      region TEXT,
      source TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS deleted_dancer_slugs (
      slug TEXT PRIMARY KEY
    );

    CREATE TABLE IF NOT EXISTS move_overrides (
      move_id TEXT PRIMARY KEY,
      patch_json TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS created_moves (
      move_id TEXT PRIMARY KEY,
      move_json TEXT NOT NULL,
      sort_order INTEGER NOT NULL
    );

    CREATE TABLE IF NOT EXISTS move_drafts (
      draft_id TEXT PRIMARY KEY,
      move_json TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      sort_order INTEGER NOT NULL
    );
  `);
}

function metadataEntryKindFromCollection(collection: string) {
  return collection === 'topics' ? 'topic' : 'family';
}

function bootstrapMetadata(db: DatabaseSync) {
  const store = readJsonFile<{ topics?: unknown[]; families?: unknown[] }>('metadata.json', {});
  const insert = db.prepare(`
    INSERT OR IGNORE INTO metadata_entries (
      id, kind, slug, name, description, created_at, updated_at, source
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  `);

  for (const collectionName of ['topics', 'families'] as const) {
    const entries = Array.isArray(store[collectionName]) ? store[collectionName] : [];
    for (const entry of entries as DatabaseRow[]) {
      if (!entry.id || !entry.slug || !entry.name) continue;
      insert.run(
        String(entry.id),
        metadataEntryKindFromCollection(collectionName),
        String(entry.slug),
        String(entry.name),
        entry.description == null ? null : String(entry.description),
        String(entry.createdAt || nowIso()),
        String(entry.updatedAt || entry.createdAt || nowIso()),
        String(entry.source || 'custom')
      );
    }
  }
}

function bootstrapDancers(db: DatabaseSync) {
  const store = readJsonFile<{ dancers?: unknown[]; deletedDancerSlugs?: unknown[] }>('dancers.json', {});
  const insertDancer = db.prepare(`
    INSERT OR IGNORE INTO dancer_profiles (
      id, slug, full_name, display_name, instagram_handle, role, level, region, source, created_at, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  `);
  const insertDeleted = db.prepare('INSERT OR IGNORE INTO deleted_dancer_slugs (slug) VALUES (?)');

  for (const dancer of (Array.isArray(store.dancers) ? store.dancers : []) as DatabaseRow[]) {
    if (!dancer.id || !dancer.slug || !dancer.fullName) continue;
    insertDancer.run(
      String(dancer.id),
      String(dancer.slug),
      String(dancer.fullName),
      String(dancer.displayName || dancer.fullName),
      dancer.instagramHandle == null ? null : String(dancer.instagramHandle),
      String(dancer.role || 'unknown'),
      String(dancer.level || 'unknown'),
      dancer.region == null ? null : String(dancer.region),
      String(dancer.source || 'custom'),
      String(dancer.createdAt || nowIso()),
      String(dancer.updatedAt || dancer.createdAt || nowIso())
    );
  }

  for (const slug of Array.isArray(store.deletedDancerSlugs) ? store.deletedDancerSlugs : []) {
    if (slug) insertDeleted.run(String(slug));
  }
}

function bootstrapMoveEdits(db: DatabaseSync) {
  const store = readJsonFile<{
    overrides?: Record<string, unknown>;
    createdMoves?: unknown[];
    drafts?: unknown[];
  }>('move-edits.json', {});
  const insertOverride = db.prepare('INSERT OR IGNORE INTO move_overrides (move_id, patch_json) VALUES (?, ?)');
  const insertCreated = db.prepare('INSERT OR IGNORE INTO created_moves (move_id, move_json, sort_order) VALUES (?, ?, ?)');
  const insertDraft = db.prepare(
    'INSERT OR IGNORE INTO move_drafts (draft_id, move_json, created_at, updated_at, sort_order) VALUES (?, ?, ?, ?, ?)'
  );

  for (const [moveId, patch] of Object.entries(store.overrides ?? {})) {
    insertOverride.run(moveId, serializeJson(patch));
  }

  (Array.isArray(store.createdMoves) ? store.createdMoves : []).forEach((move, index) => {
    const entry = move as DatabaseRow;
    if (!entry.id) return;
    insertCreated.run(String(entry.id), serializeJson(move), index);
  });

  (Array.isArray(store.drafts) ? store.drafts : []).forEach((draft, index) => {
    const entry = draft as DatabaseRow;
    if (!entry.draftId || !entry.move) return;
    insertDraft.run(
      String(entry.draftId),
      serializeJson(entry.move),
      String(entry.createdAt || nowIso()),
      String(entry.updatedAt || entry.createdAt || nowIso()),
      index
    );
  });
}

function bootstrapFromJsonSidecars(db: DatabaseSync) {
  const completed = db.prepare('SELECT value FROM app_state_meta WHERE key = ?').get('json_bootstrap_v1') as
    | { value: string }
    | undefined;
  if (completed?.value === 'complete') {
    return;
  }

  db.exec('BEGIN IMMEDIATE');
  try {
    bootstrapMetadata(db);
    bootstrapDancers(db);
    bootstrapMoveEdits(db);
    db.prepare('INSERT OR REPLACE INTO app_state_meta (key, value) VALUES (?, ?)').run('json_bootstrap_v1', 'complete');
    db.exec('COMMIT');
  } catch (error) {
    db.exec('ROLLBACK');
    throw error;
  }
}

function ensureDatabase() {
  if (database) {
    return database;
  }

  fs.mkdirSync(resolveDataDir(), { recursive: true });
  database = new DatabaseSync(dbPath());
  database.exec('PRAGMA journal_mode = WAL');
  database.exec('PRAGMA foreign_keys = ON');
  createSchema(database);
  return database;
}

export function getAppDatabase() {
  const db = ensureDatabase();
  if (!initialized) {
    bootstrapFromJsonSidecars(db);
    initialized = true;
  }
  return db;
}

export function runInTransaction<T>(callback: (db: DatabaseSync) => T) {
  const db = getAppDatabase();
  db.exec('BEGIN IMMEDIATE');
  try {
    const result = callback(db);
    db.exec('COMMIT');
    return result;
  } catch (error) {
    db.exec('ROLLBACK');
    throw error;
  }
}

export function recordAction(db: DatabaseSync, input: ActionInput) {
  const id = randomUUID();
  const createdAt = nowIso();
  db.prepare(`
    INSERT INTO actions (
      id, type, label, entity_type, entity_id, status, created_at, actor,
      before_json, after_json, undone_by_action_id, undo_of_action_id
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  `).run(
    id,
    input.type,
    input.label,
    input.entityType,
    input.entityId,
    'active',
    createdAt,
    input.actor ?? null,
    serializeJson(input.before),
    serializeJson(input.after),
    null,
    input.undoOfActionId ?? null
  );

  return {
    id,
    createdAt
  };
}

export function listActions(limit = 100): ActionRecord[] {
  const db = getAppDatabase();
  const rows = statementRows<DatabaseRow>(
    db.prepare(`
      SELECT
        id, type, label, entity_type, entity_id, status, created_at, actor,
        before_json, after_json, undone_by_action_id, undo_of_action_id
      FROM actions
      ORDER BY created_at DESC
      LIMIT ?
    `),
    Math.max(1, Math.min(250, Math.floor(limit)))
  );

  return rows.map((row) => ({
    id: String(row.id),
    type: String(row.type),
    label: String(row.label),
    entityType: String(row.entity_type),
    entityId: String(row.entity_id),
    status: row.status === 'undone' ? 'undone' : 'active',
    createdAt: String(row.created_at),
    actor: row.actor == null ? null : String(row.actor),
    before: parseJson(row.before_json),
    after: parseJson(row.after_json),
    undoneByActionId: row.undone_by_action_id == null ? null : String(row.undone_by_action_id),
    undoOfActionId: row.undo_of_action_id == null ? null : String(row.undo_of_action_id)
  }));
}

export function getAction(id: string) {
  return listActions(250).find((action) => action.id === id) ?? null;
}

export function markActionUndone(db: DatabaseSync, actionId: string, undoneByActionId: string) {
  db.prepare('UPDATE actions SET status = ?, undone_by_action_id = ? WHERE id = ?').run(
    'undone',
    undoneByActionId,
    actionId
  );
}
