import fs from 'node:fs';
import path from 'node:path';
import { randomUUID } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { AsyncLocalStorage } from 'node:async_hooks';
import { resolveAppStateBootstrapDir, resolveCatalogBootstrapDir, resolveDataDir } from './paths';

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
const actionActorStorage = new AsyncLocalStorage<string | null>();

function nowIso() {
  return new Date().toISOString();
}

function dbPath() {
  return path.join(resolveDataDir(), DB_FILENAME);
}

function readJsonFileFromDirectory<T>(directory: string, filename: string, fallback: T): T {
  try {
    const contents = fs.readFileSync(path.join(directory, filename), 'utf-8');
    return JSON.parse(contents) as T;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      try {
        const contents = fs.readFileSync(path.join(resolveDataDir(), filename), 'utf-8');
        return JSON.parse(contents) as T;
      } catch (fallbackError) {
        if ((fallbackError as NodeJS.ErrnoException).code === 'ENOENT') {
          return fallback;
        }
        throw fallbackError;
      }
    }
    throw error;
  }
}

function readAppStateBootstrapJson<T>(filename: string, fallback: T): T {
  return readJsonFileFromDirectory(resolveAppStateBootstrapDir(), filename, fallback);
}

function readCatalogBootstrapJson<T>(filename: string, fallback: T): T {
  return readJsonFileFromDirectory(resolveCatalogBootstrapDir(), filename, fallback);
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

    CREATE TABLE IF NOT EXISTS auth_users (
      id TEXT PRIMARY KEY,
      username TEXT NOT NULL,
      username_normalized TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      role TEXT NOT NULL,
      is_active INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      last_login_at TEXT
    );

    CREATE INDEX IF NOT EXISTS idx_auth_users_role ON auth_users(role);

    CREATE TABLE IF NOT EXISTS auth_sessions (
      id TEXT PRIMARY KEY,
      user_id TEXT NOT NULL,
      token_hash TEXT NOT NULL UNIQUE,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      last_seen_at TEXT NOT NULL,
      FOREIGN KEY(user_id) REFERENCES auth_users(id) ON DELETE CASCADE
    );

    CREATE INDEX IF NOT EXISTS idx_auth_sessions_token_hash ON auth_sessions(token_hash);
    CREATE INDEX IF NOT EXISTS idx_auth_sessions_user ON auth_sessions(user_id);
    CREATE INDEX IF NOT EXISTS idx_auth_sessions_expires_at ON auth_sessions(expires_at);

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

    CREATE TABLE IF NOT EXISTS media_jobs (
      id TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      target_type TEXT NOT NULL,
      target_id TEXT NOT NULL,
      status TEXT NOT NULL,
      idempotency_key TEXT NOT NULL UNIQUE,
      payload_json TEXT NOT NULL,
      attempts INTEGER NOT NULL,
      max_attempts INTEGER NOT NULL,
      error TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      started_at TEXT,
      finished_at TEXT
    );

    CREATE INDEX IF NOT EXISTS idx_media_jobs_status ON media_jobs(status, updated_at);
    CREATE INDEX IF NOT EXISTS idx_media_jobs_target ON media_jobs(target_type, target_id);

    CREATE TABLE IF NOT EXISTS media_file_actions (
      id TEXT PRIMARY KEY,
      job_id TEXT NOT NULL,
      action_type TEXT NOT NULL,
      status TEXT NOT NULL,
      file_path TEXT NOT NULL,
      backup_path TEXT,
      metadata_json TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY(job_id) REFERENCES media_jobs(id)
    );

    CREATE INDEX IF NOT EXISTS idx_media_file_actions_job ON media_file_actions(job_id);

    CREATE TABLE IF NOT EXISTS media_video_assets (
      id TEXT PRIMARY KEY,
      kind TEXT NOT NULL,
      file_path TEXT NOT NULL UNIQUE,
      display_name TEXT NOT NULL,
      original_filename TEXT NOT NULL,
      dancers_json TEXT NOT NULL,
      timing TEXT NOT NULL,
      content_type TEXT NOT NULL,
      environment TEXT NOT NULL,
      origin_type TEXT NOT NULL,
      source_url TEXT,
      record_date TEXT,
      class_workshop TEXT,
      tags_json TEXT NOT NULL,
      notes TEXT,
      content_hash TEXT,
      content_hash_algorithm TEXT,
      content_size_bytes INTEGER,
      hash_status TEXT NOT NULL,
      created_at TEXT NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_media_video_assets_kind ON media_video_assets(kind);
    CREATE INDEX IF NOT EXISTS idx_media_video_assets_content_hash ON media_video_assets(content_hash, content_size_bytes);

    CREATE TABLE IF NOT EXISTS media_move_video_links (
      id TEXT PRIMARY KEY,
      move_id TEXT NOT NULL,
      asset_id TEXT NOT NULL,
      sort_order INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      FOREIGN KEY(asset_id) REFERENCES media_video_assets(id)
    );

    CREATE INDEX IF NOT EXISTS idx_media_move_video_links_move ON media_move_video_links(move_id, sort_order);
    CREATE INDEX IF NOT EXISTS idx_media_move_video_links_asset ON media_move_video_links(asset_id);

    CREATE TABLE IF NOT EXISTS media_derived_clips (
      id TEXT PRIMARY KEY,
      source_asset_id TEXT NOT NULL,
      move_id TEXT NOT NULL,
      move_display_id TEXT,
      is_key_video INTEGER NOT NULL,
      label TEXT,
      descriptor_label TEXT,
      start_position_id TEXT,
      end_position_id TEXT,
      timing_group_id TEXT,
      manually_named INTEGER NOT NULL,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      action_start_ms INTEGER,
      action_end_ms INTEGER,
      crop_rect_json TEXT,
      count_markers_json TEXT NOT NULL,
      count_overlay_placement TEXT NOT NULL,
      count_timing_preset TEXT NOT NULL,
      output_asset_id TEXT,
      action_output_file_path TEXT,
      low_res_output_file_path TEXT,
      low_res_padded_output_file_path TEXT,
      published_asset_id TEXT,
      published_action_output_file_path TEXT,
      published_low_res_file_path TEXT,
      published_low_res_padded_file_path TEXT,
      published_at TEXT,
      status TEXT NOT NULL,
      error TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY(source_asset_id) REFERENCES media_video_assets(id)
    );

    CREATE INDEX IF NOT EXISTS idx_media_derived_clips_source ON media_derived_clips(source_asset_id);
    CREATE INDEX IF NOT EXISTS idx_media_derived_clips_move ON media_derived_clips(move_id);

    CREATE TABLE IF NOT EXISTS catalog_manifest (
      id TEXT PRIMARY KEY,
      manifest_json TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS catalog_moves (
      move_id TEXT PRIMARY KEY,
      sort_order INTEGER NOT NULL,
      move_json TEXT NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_catalog_moves_sort ON catalog_moves(sort_order);

    CREATE TABLE IF NOT EXISTS catalog_layout_columns (
      column_number INTEGER PRIMARY KEY,
      sort_order INTEGER NOT NULL,
      column_json TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS catalog_progress_snapshots (
      snapshot_date TEXT PRIMARY KEY,
      sort_order INTEGER NOT NULL,
      snapshot_json TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS catalog_raw_move_references (
      row_key TEXT PRIMARY KEY,
      move_id TEXT,
      sort_order INTEGER NOT NULL,
      reference_json TEXT NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_catalog_raw_move_references_sort ON catalog_raw_move_references(sort_order);

    CREATE TABLE IF NOT EXISTS position_options (
      id TEXT PRIMARY KEY,
      label TEXT NOT NULL,
      sort_order INTEGER NOT NULL
    );
  `);
}

function metadataEntryKindFromCollection(collection: string) {
  return collection === 'topics' ? 'topic' : 'family';
}

function bootstrapMetadata(db: DatabaseSync) {
  const store = readAppStateBootstrapJson<{ topics?: unknown[]; families?: unknown[] }>('metadata.json', {});
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
  const store = readAppStateBootstrapJson<{ dancers?: unknown[]; deletedDancerSlugs?: unknown[] }>('dancers.json', {});
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
  const store = readAppStateBootstrapJson<{
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

function bootstrapPositions(db: DatabaseSync) {
  const store = readAppStateBootstrapJson<{ positions?: unknown[] }>('positions.json', {});
  const insert = db.prepare('INSERT OR IGNORE INTO position_options (id, label, sort_order) VALUES (?, ?, ?)');

  (Array.isArray(store.positions) ? store.positions : []).forEach((position, index) => {
    const raw = position as DatabaseRow | string;
    const id = typeof raw === 'string' ? raw : raw.id || raw.label || raw.name;
    const label = typeof raw === 'string' ? raw : raw.label || raw.name || raw.id;
    if (!id || !label) return;
    insert.run(String(id), String(label), index);
  });
}

function catalogTableHasRows(db: DatabaseSync) {
  const row = db.prepare('SELECT COUNT(*) AS count FROM catalog_moves').get() as { count?: number | bigint } | undefined;
  return Number(row?.count ?? 0) > 0;
}

function bootstrapCatalogData(db: DatabaseSync) {
  const completed = db.prepare('SELECT value FROM app_state_meta WHERE key = ?').get('catalog_json_bootstrap_v1') as
    | { value: string }
    | undefined;
  if (completed?.value === 'complete') {
    return;
  }

  if (catalogTableHasRows(db)) {
    db.prepare('INSERT OR REPLACE INTO app_state_meta (key, value) VALUES (?, ?)').run(
      'catalog_json_bootstrap_v1',
      'complete'
    );
    return;
  }

  const manifest = readCatalogBootstrapJson('manifest.json', null);
  const moves = readCatalogBootstrapJson<unknown[]>('moves.json', []);
  const layout = readCatalogBootstrapJson<unknown[]>('layout.json', []);
  const progress = readCatalogBootstrapJson<unknown[]>('progress.json', []);
  const rawMoves = readCatalogBootstrapJson<unknown[]>('raw-moves.json', []);

  if (manifest) {
    db.prepare('INSERT OR REPLACE INTO catalog_manifest (id, manifest_json) VALUES (?, ?)').run(
      'main',
      serializeJson(manifest)
    );
  }

  const insertMove = db.prepare('INSERT OR REPLACE INTO catalog_moves (move_id, sort_order, move_json) VALUES (?, ?, ?)');
  moves.forEach((move, index) => {
    const entry = move as DatabaseRow;
    if (!entry.id) return;
    insertMove.run(String(entry.id), index, serializeJson(move));
  });

  const insertLayout = db.prepare(
    'INSERT OR REPLACE INTO catalog_layout_columns (column_number, sort_order, column_json) VALUES (?, ?, ?)'
  );
  layout.forEach((column, index) => {
    const entry = column as DatabaseRow;
    const columnNumber = Number(entry.column ?? index + 1);
    if (!Number.isFinite(columnNumber)) return;
    insertLayout.run(Math.floor(columnNumber), index, serializeJson(column));
  });

  const insertProgress = db.prepare(
    'INSERT OR REPLACE INTO catalog_progress_snapshots (snapshot_date, sort_order, snapshot_json) VALUES (?, ?, ?)'
  );
  progress.forEach((snapshot, index) => {
    const entry = snapshot as DatabaseRow;
    if (!entry.date) return;
    insertProgress.run(String(entry.date), index, serializeJson(snapshot));
  });

  const insertRawMove = db.prepare(
    'INSERT OR REPLACE INTO catalog_raw_move_references (row_key, move_id, sort_order, reference_json) VALUES (?, ?, ?, ?)'
  );
  rawMoves.forEach((reference, index) => {
    const entry = reference as DatabaseRow;
    insertRawMove.run(`row-${index}`, entry.id == null ? null : String(entry.id), index, serializeJson(reference));
  });

  db.prepare('INSERT OR REPLACE INTO app_state_meta (key, value) VALUES (?, ?)').run(
    'catalog_json_bootstrap_v1',
    'complete'
  );
}

function bootstrapFromJsonSidecars(db: DatabaseSync) {
  const completed = db.prepare('SELECT value FROM app_state_meta WHERE key = ?').get('json_bootstrap_v1') as
    | { value: string }
    | undefined;
  if (completed?.value === 'complete') {
    db.exec('BEGIN IMMEDIATE');
    try {
      bootstrapCatalogData(db);
      db.exec('COMMIT');
    } catch (error) {
      db.exec('ROLLBACK');
      throw error;
    }
    return;
  }

  db.exec('BEGIN IMMEDIATE');
  try {
    bootstrapMetadata(db);
    bootstrapDancers(db);
    bootstrapMoveEdits(db);
    bootstrapPositions(db);
    bootstrapCatalogData(db);
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

export function runWithActionActor<T>(actor: string | null | undefined, callback: () => T): T {
  return actionActorStorage.run(actor ?? null, callback);
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
    input.actor ?? actionActorStorage.getStore() ?? null,
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
