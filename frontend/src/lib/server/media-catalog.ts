import fs from 'node:fs/promises';
import path from 'node:path';
import { randomUUID } from 'node:crypto';
import type { DatabaseSync } from 'node:sqlite';
import type {
  ClipCountMarker,
  ClipCropRect,
  CountOverlayPlacement,
  CountTimingPreset,
  DerivedClip,
  MediaHashAlgorithm,
  MediaHashStatus,
  MoveVideoLink,
  VideoAsset,
  VideoContentType,
  VideoEnvironment,
  VideoLibrary,
  VideoOriginType,
  VideoTiming
} from '$lib/types';
import {
  normalizeDateString,
  normalizeOptionalText,
  normalizeTags
} from '$lib/video-library-utils';
import {
  normalizeManagedVideoPath,
  resolveAppStateBootstrapDir,
  resolveDataDir,
  resolveMediaRoot,
  resolvePosterRoot,
  resolveSourceRoot
} from './paths';
import { getAppDatabase } from './app-state';

const LIBRARY_FILENAME = 'video-library.json';
const LIBRARY_EXPORT_DIRNAME = 'media-catalog-exports';
const SQLITE_BOOTSTRAP_META_KEY = 'media_catalog_sqlite_v1';

let libraryWriteQueue: Promise<unknown> = Promise.resolve();
let bootstrapPromise: Promise<void> | null = null;

type DatabaseRow = Record<string, unknown>;

function nowIso() {
  return new Date().toISOString();
}

function safeDisplayName(filename: string) {
  return path.parse(filename).name.replace(/[_-]+/g, ' ').trim() || path.parse(filename).name;
}

function libraryFilePath() {
  return path.join(resolveAppStateBootstrapDir(), LIBRARY_FILENAME);
}

function mediaCatalogExportDir() {
  return path.join(resolveDataDir(), LIBRARY_EXPORT_DIRNAME);
}

function exportTimestamp(value: Date) {
  return value.toISOString().replace(/[:.]/g, '-');
}

function isVideoTiming(value: unknown): value is VideoTiming {
  return value === 'on1' || value === 'on2' || value === 'other';
}

function isVideoContentType(value: unknown): value is VideoContentType {
  return value === 'music' || value === 'counts' || value === 'other';
}

function isVideoEnvironment(value: unknown): value is VideoEnvironment {
  return value === 'social' || value === 'class';
}

function isVideoOriginType(value: unknown): value is VideoOriginType {
  return value === 'self-recorded' || value === 'download';
}

function normalizeContentHash(value: unknown) {
  const text = typeof value === 'string' ? value.trim().toLowerCase() : '';
  return /^sha256:[a-f0-9]{64}$/.test(text) ? text : null;
}

function normalizeHashAlgorithm(value: unknown): MediaHashAlgorithm | null {
  return value === 'sha256' ? 'sha256' : null;
}

function normalizeHashStatus(value: unknown, contentHash: string | null): MediaHashStatus {
  if (value === 'failed') return 'failed';
  if (contentHash) return 'ready';
  return 'pending';
}

function normalizeContentSizeBytes(value: unknown) {
  const numberValue = Number(value);
  return Number.isSafeInteger(numberValue) && numberValue >= 0 ? numberValue : null;
}

function isCountOverlayPlacement(value: unknown): value is CountOverlayPlacement {
  return value === 'top-left' || value === 'top-right' || value === 'bottom-left' || value === 'bottom-right';
}

function isCountTimingPreset(value: unknown): value is CountTimingPreset {
  return value === 'on2-default' || value === 'on2-all' || value === 'on1-default' || value === 'on1-all';
}

function normalizeCropRect(value: unknown): ClipCropRect | null {
  if (!value || typeof value !== 'object') {
    return null;
  }

  const raw = value as Partial<ClipCropRect>;
  const x = Number(raw.x);
  const y = Number(raw.y);
  const width = Number(raw.width);
  const height = Number(raw.height);
  if (![x, y, width, height].every(Number.isFinite) || width <= 0 || height <= 0) {
    return null;
  }

  return {
    x: Math.max(0, Math.min(1, x)),
    y: Math.max(0, Math.min(1, y)),
    width: Math.max(0.01, Math.min(1, width)),
    height: Math.max(0.01, Math.min(1, height))
  };
}

function normalizeCountMarkers(value: unknown): ClipCountMarker[] {
  if (!Array.isArray(value)) {
    return [];
  }

  return value
    .map((entry) => {
      if (!entry || typeof entry !== 'object') {
        return null;
      }

      const raw = entry as Partial<ClipCountMarker>;
      const count = String(raw.count ?? '').trim();
      const ms = Math.max(0, Math.floor(Number(raw.ms ?? 0)));
      if (!count || !Number.isFinite(ms)) {
        return null;
      }

      return {
        id: String(raw.id || randomUUID()),
        count,
        ms,
        clear: Boolean(raw.clear)
      };
    })
    .filter((entry): entry is ClipCountMarker => Boolean(entry))
    .sort((left, right) => left.ms - right.ms);
}

function inferMetadataFromText(value: string) {
  const text = value.toLowerCase();
  const timing: VideoTiming = /\bon\s*1\b|\[on1\b| on1[,\]]/.test(text)
    ? 'on1'
    : /\bon\s*2\b|\[on2\b| on2[,\]]/.test(text)
      ? 'on2'
      : 'other';
  const contentType: VideoContentType = text.includes('count') ? 'counts' : text.includes('music') ? 'music' : 'other';
  const environment: VideoEnvironment = text.includes('social') ? 'social' : 'class';
  return { timing, contentType, environment };
}

function normalizeLegacyMetadata(raw: Partial<VideoAsset> & { sourceType?: string }) {
  const sourceType = raw.sourceType;
  const inferred = inferMetadataFromText(`${raw.displayName ?? ''} ${raw.originalFilename ?? ''}`);
  return {
    timing: isVideoTiming(raw.timing) ? raw.timing : inferred.timing,
    contentType: isVideoContentType(raw.contentType)
      ? raw.contentType
      : sourceType === 'music' || sourceType === 'counts'
        ? sourceType
        : inferred.contentType,
    environment: isVideoEnvironment(raw.environment)
      ? raw.environment
      : sourceType === 'social-dance'
        ? 'social'
        : inferred.environment
  };
}

function normalizeVideoAsset(raw: Partial<VideoAsset> & { sourceType?: string }): VideoAsset | null {
  if (!raw.id || !raw.kind || !raw.filePath || !raw.originalFilename) {
    return null;
  }

  const metadata = normalizeLegacyMetadata(raw);
  const originType = isVideoOriginType(raw.originType) ? raw.originType : 'self-recorded';
  const contentHash = normalizeContentHash(raw.contentHash);
  return {
    id: String(raw.id),
    kind: raw.kind,
    filePath: normalizeManagedVideoPath(String(raw.filePath)),
    displayName: String(raw.displayName || safeDisplayName(String(raw.originalFilename))),
    originalFilename: String(raw.originalFilename),
    dancers: Array.isArray(raw.dancers) ? raw.dancers.map(String).filter(Boolean) : [],
    timing: metadata.timing,
    contentType: metadata.contentType,
    environment: metadata.environment,
    originType,
    sourceUrl: originType === 'download' ? normalizeOptionalText(raw.sourceUrl) : null,
    recordDate: normalizeDateString(raw.recordDate),
    classWorkshop: normalizeOptionalText(raw.classWorkshop),
    tags: normalizeTags(raw.tags),
    notes: raw.notes ? String(raw.notes) : null,
    contentHash,
    contentHashAlgorithm: contentHash ? normalizeHashAlgorithm(raw.contentHashAlgorithm) ?? 'sha256' : null,
    contentSizeBytes: normalizeContentSizeBytes(raw.contentSizeBytes),
    hashStatus: normalizeHashStatus(raw.hashStatus, contentHash),
    createdAt: String(raw.createdAt || nowIso())
  };
}

function isDerivedClipStatus(value: unknown): DerivedClip['status'] {
  if (value === 'rendering' || value === 'ready' || value === 'failed') {
    return value;
  }

  return 'pending';
}

function normalizeDerivedClip(raw: Partial<DerivedClip>, library: { moveVideoLinks?: MoveVideoLink[] }): DerivedClip | null {
  if (!raw.id || !raw.sourceAssetId || !raw.moveId) {
    return null;
  }

  const outputAssetId = raw.outputAssetId ? String(raw.outputAssetId) : null;
  const linkedOutput = outputAssetId
    ? library.moveVideoLinks?.some((link) => link.assetId === outputAssetId && link.moveId === String(raw.moveId).trim().toUpperCase())
    : false;
  const publishedAssetId = raw.publishedAssetId
    ? String(raw.publishedAssetId)
    : linkedOutput
      ? outputAssetId
      : null;
  const createdAt = String(raw.createdAt || nowIso());

  return {
    id: String(raw.id),
    sourceAssetId: String(raw.sourceAssetId),
    moveId: String(raw.moveId).trim().toUpperCase(),
    moveDisplayId: raw.moveDisplayId ? String(raw.moveDisplayId).trim().toUpperCase() : null,
    isKeyVideo: Boolean(raw.isKeyVideo),
    label: raw.label ? String(raw.label) : null,
    descriptorLabel: normalizeOptionalText(raw.descriptorLabel),
    startPositionId: normalizeOptionalText(raw.startPositionId),
    endPositionId: normalizeOptionalText(raw.endPositionId),
    timingGroupId: normalizeOptionalText(raw.timingGroupId),
    manuallyNamed: Boolean(raw.manuallyNamed),
    startMs: Math.max(0, Math.floor(Number(raw.startMs ?? 0))),
    endMs: Math.max(0, Math.floor(Number(raw.endMs ?? 0))),
    actionStartMs: raw.actionStartMs === null || raw.actionStartMs === undefined ? null : Math.max(0, Math.floor(Number(raw.actionStartMs))),
    actionEndMs: raw.actionEndMs === null || raw.actionEndMs === undefined ? null : Math.max(0, Math.floor(Number(raw.actionEndMs))),
    cropRect: normalizeCropRect(raw.cropRect),
    countMarkers: normalizeCountMarkers(raw.countMarkers),
    countOverlayPlacement: isCountOverlayPlacement(raw.countOverlayPlacement) ? raw.countOverlayPlacement : 'top-left',
    countTimingPreset: isCountTimingPreset(raw.countTimingPreset) ? raw.countTimingPreset : 'on2-default',
    outputAssetId,
    actionOutputFilePath: raw.actionOutputFilePath ? normalizeManagedVideoPath(String(raw.actionOutputFilePath)) : null,
    lowResOutputFilePath: raw.lowResOutputFilePath ? normalizeManagedVideoPath(String(raw.lowResOutputFilePath)) : null,
    lowResPaddedOutputFilePath: raw.lowResPaddedOutputFilePath ? normalizeManagedVideoPath(String(raw.lowResPaddedOutputFilePath)) : null,
    publishedAssetId,
    publishedActionOutputFilePath: raw.publishedActionOutputFilePath
      ? normalizeManagedVideoPath(String(raw.publishedActionOutputFilePath))
      : null,
    publishedLowResFilePath: raw.publishedLowResFilePath ? normalizeManagedVideoPath(String(raw.publishedLowResFilePath)) : null,
    publishedLowResPaddedFilePath: raw.publishedLowResPaddedFilePath
      ? normalizeManagedVideoPath(String(raw.publishedLowResPaddedFilePath))
      : null,
    publishedAt: raw.publishedAt ? String(raw.publishedAt) : publishedAssetId ? createdAt : null,
    status: isDerivedClipStatus(raw.status),
    error: raw.error ? String(raw.error) : null,
    createdAt,
    updatedAt: String(raw.updatedAt || createdAt)
  };
}

export function defaultMediaCatalog(): VideoLibrary {
  return {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  };
}

export async function ensureMediaCatalogRoots() {
  await Promise.all([
    fs.mkdir(resolveDataDir(), { recursive: true }),
    fs.mkdir(resolveMediaRoot(), { recursive: true }),
    fs.mkdir(resolveSourceRoot(), { recursive: true }),
    fs.mkdir(resolvePosterRoot(), { recursive: true })
  ]);
}

function serializeJson(value: unknown) {
  return JSON.stringify(value ?? null);
}

function parseJson(value: unknown, fallback: unknown) {
  if (typeof value !== 'string' || !value) {
    return fallback;
  }

  try {
    return JSON.parse(value);
  } catch {
    return fallback;
  }
}

function sqliteBoolean(value: unknown) {
  return value ? 1 : 0;
}

function sqlNullable(value: string | number | null | undefined) {
  return value ?? null;
}

function fromSqliteBoolean(value: unknown) {
  return Number(value) === 1;
}

function statementRows<T>(statement: ReturnType<DatabaseSync['prepare']>, ...params: any[]): T[] {
  return statement.all(...params) as T[];
}

async function readMediaCatalogFromJsonSeed() {
  const filePath = libraryFilePath();
  let contents: string;

  try {
    contents = await fs.readFile(filePath, 'utf-8');
  } catch {
    return { library: defaultMediaCatalog(), sourceExists: false, sourceContents: null };
  }

  try {
    const parsed = JSON.parse(contents) as Partial<VideoLibrary>;
    const library: VideoLibrary = {
      videoAssets: Array.isArray(parsed.videoAssets)
        ? parsed.videoAssets.map((asset) => normalizeVideoAsset(asset)).filter((asset): asset is VideoAsset => Boolean(asset))
        : [],
      moveVideoLinks: Array.isArray(parsed.moveVideoLinks) ? parsed.moveVideoLinks : [],
      derivedClips: Array.isArray(parsed.derivedClips)
        ? parsed.derivedClips.map((clip) => normalizeDerivedClip(clip, parsed)).filter((clip): clip is DerivedClip => Boolean(clip))
        : []
    };

    sortMediaCatalog(library);
    return { library, sourceExists: true, sourceContents: contents };
  } catch {
    return { library: defaultMediaCatalog(), sourceExists: true, sourceContents: contents };
  }
}

function readMediaCatalogFromSqlite(db: DatabaseSync): VideoLibrary {
  const videoAssets = statementRows<DatabaseRow>(
    db.prepare(`
      SELECT
        id, kind, file_path, display_name, original_filename, dancers_json, timing, content_type, environment,
        origin_type, source_url, record_date, class_workshop, tags_json, notes, content_hash,
        content_hash_algorithm, content_size_bytes, hash_status, created_at
      FROM media_video_assets
      ORDER BY created_at, id
    `)
  )
    .map((row) =>
      normalizeVideoAsset({
        id: String(row.id),
        kind: row.kind as VideoAsset['kind'],
        filePath: String(row.file_path),
        displayName: String(row.display_name),
        originalFilename: String(row.original_filename),
        dancers: parseJson(row.dancers_json, []),
        timing: row.timing as VideoTiming,
        contentType: row.content_type as VideoContentType,
        environment: row.environment as VideoEnvironment,
        originType: row.origin_type as VideoOriginType,
        sourceUrl: row.source_url == null ? null : String(row.source_url),
        recordDate: row.record_date == null ? null : String(row.record_date),
        classWorkshop: row.class_workshop == null ? null : String(row.class_workshop),
        tags: parseJson(row.tags_json, []),
        notes: row.notes == null ? null : String(row.notes),
        contentHash: row.content_hash == null ? null : String(row.content_hash),
        contentHashAlgorithm:
          row.content_hash_algorithm == null ? null : (String(row.content_hash_algorithm) as MediaHashAlgorithm),
        contentSizeBytes: row.content_size_bytes == null ? null : Number(row.content_size_bytes),
        hashStatus: row.hash_status as MediaHashStatus,
        createdAt: String(row.created_at)
      })
    )
    .filter((asset): asset is VideoAsset => Boolean(asset));

  const moveVideoLinks = statementRows<DatabaseRow>(
    db.prepare(`
      SELECT id, move_id, asset_id, sort_order, created_at
      FROM media_move_video_links
      ORDER BY move_id, sort_order, id
    `)
  ).map((row) => ({
    id: String(row.id),
    moveId: String(row.move_id),
    assetId: String(row.asset_id),
    order: Math.floor(Number(row.sort_order)),
    createdAt: String(row.created_at)
  }));

  const derivedClips = statementRows<DatabaseRow>(
    db.prepare(`
      SELECT
        id, source_asset_id, move_id, move_display_id, is_key_video, label, descriptor_label,
        start_position_id, end_position_id, timing_group_id, manually_named, start_ms, end_ms,
        action_start_ms, action_end_ms, crop_rect_json, count_markers_json, count_overlay_placement,
        count_timing_preset, output_asset_id, action_output_file_path, low_res_output_file_path,
        low_res_padded_output_file_path, published_asset_id, published_action_output_file_path,
        published_low_res_file_path, published_low_res_padded_file_path, published_at, status,
        error, created_at, updated_at
      FROM media_derived_clips
      ORDER BY source_asset_id, start_ms, id
    `)
  )
    .map((row) =>
      normalizeDerivedClip(
        {
          id: String(row.id),
          sourceAssetId: String(row.source_asset_id),
          moveId: String(row.move_id),
          moveDisplayId: row.move_display_id == null ? null : String(row.move_display_id),
          isKeyVideo: fromSqliteBoolean(row.is_key_video),
          label: row.label == null ? null : String(row.label),
          descriptorLabel: row.descriptor_label == null ? null : String(row.descriptor_label),
          startPositionId: row.start_position_id == null ? null : String(row.start_position_id),
          endPositionId: row.end_position_id == null ? null : String(row.end_position_id),
          timingGroupId: row.timing_group_id == null ? null : String(row.timing_group_id),
          manuallyNamed: fromSqliteBoolean(row.manually_named),
          startMs: Number(row.start_ms),
          endMs: Number(row.end_ms),
          actionStartMs: row.action_start_ms == null ? null : Number(row.action_start_ms),
          actionEndMs: row.action_end_ms == null ? null : Number(row.action_end_ms),
          cropRect: parseJson(row.crop_rect_json, null) as ClipCropRect | null,
          countMarkers: parseJson(row.count_markers_json, []) as ClipCountMarker[],
          countOverlayPlacement: row.count_overlay_placement as CountOverlayPlacement,
          countTimingPreset: row.count_timing_preset as CountTimingPreset,
          outputAssetId: row.output_asset_id == null ? null : String(row.output_asset_id),
          actionOutputFilePath: row.action_output_file_path == null ? null : String(row.action_output_file_path),
          lowResOutputFilePath: row.low_res_output_file_path == null ? null : String(row.low_res_output_file_path),
          lowResPaddedOutputFilePath:
            row.low_res_padded_output_file_path == null ? null : String(row.low_res_padded_output_file_path),
          publishedAssetId: row.published_asset_id == null ? null : String(row.published_asset_id),
          publishedActionOutputFilePath:
            row.published_action_output_file_path == null ? null : String(row.published_action_output_file_path),
          publishedLowResFilePath: row.published_low_res_file_path == null ? null : String(row.published_low_res_file_path),
          publishedLowResPaddedFilePath:
            row.published_low_res_padded_file_path == null ? null : String(row.published_low_res_padded_file_path),
          publishedAt: row.published_at == null ? null : String(row.published_at),
          status: row.status as DerivedClip['status'],
          error: row.error == null ? null : String(row.error),
          createdAt: String(row.created_at),
          updatedAt: String(row.updated_at)
        },
        { moveVideoLinks }
      )
    )
    .filter((clip): clip is DerivedClip => Boolean(clip));

  const library = { videoAssets, moveVideoLinks, derivedClips };
  sortMediaCatalog(library);
  return library;
}

function replaceMediaCatalogInTransaction(db: DatabaseSync, input: VideoLibrary) {
  const library = structuredClone(input);
  sortMediaCatalog(library);

  db.prepare('DELETE FROM media_derived_clips').run();
  db.prepare('DELETE FROM media_move_video_links').run();
  db.prepare('DELETE FROM media_video_assets').run();

  const insertAsset = db.prepare(`
    INSERT INTO media_video_assets (
      id, kind, file_path, display_name, original_filename, dancers_json, timing, content_type,
      environment, origin_type, source_url, record_date, class_workshop, tags_json, notes,
      content_hash, content_hash_algorithm, content_size_bytes, hash_status, created_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  `);

  for (const asset of library.videoAssets
    .map((entry) => normalizeVideoAsset(entry))
    .filter((entry): entry is VideoAsset => Boolean(entry))) {
    insertAsset.run(
      asset.id,
      asset.kind,
      normalizeManagedVideoPath(asset.filePath),
      asset.displayName,
      asset.originalFilename,
      serializeJson(asset.dancers),
      asset.timing,
      asset.contentType,
      asset.environment,
      asset.originType,
      asset.sourceUrl,
      asset.recordDate,
      asset.classWorkshop,
      serializeJson(asset.tags),
      asset.notes,
      asset.contentHash,
      asset.contentHashAlgorithm,
      asset.contentSizeBytes,
      asset.hashStatus,
      asset.createdAt
    );
  }

  const insertLink = db.prepare(`
    INSERT INTO media_move_video_links (id, move_id, asset_id, sort_order, created_at)
    VALUES (?, ?, ?, ?, ?)
  `);

  for (const link of library.moveVideoLinks) {
    if (!link.id || !link.moveId || !link.assetId) {
      continue;
    }
    insertLink.run(
      String(link.id),
      String(link.moveId).trim().toUpperCase(),
      String(link.assetId),
      Math.floor(Number(link.order ?? 0)),
      String(link.createdAt || nowIso())
    );
  }

  const normalizedClips = library.derivedClips
    .map((entry) => normalizeDerivedClip(entry, library))
    .filter((entry): entry is DerivedClip => Boolean(entry));
  const insertClip = db.prepare(`
    INSERT INTO media_derived_clips (
      id, source_asset_id, move_id, move_display_id, is_key_video, label, descriptor_label,
      start_position_id, end_position_id, timing_group_id, manually_named, start_ms, end_ms,
      action_start_ms, action_end_ms, crop_rect_json, count_markers_json, count_overlay_placement,
      count_timing_preset, output_asset_id, action_output_file_path, low_res_output_file_path,
      low_res_padded_output_file_path, published_asset_id, published_action_output_file_path,
      published_low_res_file_path, published_low_res_padded_file_path, published_at, status,
      error, created_at, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  `);

  for (const clip of normalizedClips) {
    insertClip.run(
      clip.id,
      clip.sourceAssetId,
      clip.moveId,
      sqlNullable(clip.moveDisplayId),
      sqliteBoolean(clip.isKeyVideo),
      sqlNullable(clip.label),
      sqlNullable(clip.descriptorLabel),
      sqlNullable(clip.startPositionId),
      sqlNullable(clip.endPositionId),
      sqlNullable(clip.timingGroupId),
      sqliteBoolean(clip.manuallyNamed),
      clip.startMs,
      clip.endMs,
      sqlNullable(clip.actionStartMs),
      sqlNullable(clip.actionEndMs),
      clip.cropRect ? serializeJson(clip.cropRect) : null,
      serializeJson(clip.countMarkers),
      clip.countOverlayPlacement,
      clip.countTimingPreset,
      sqlNullable(clip.outputAssetId),
      sqlNullable(clip.actionOutputFilePath),
      sqlNullable(clip.lowResOutputFilePath),
      sqlNullable(clip.lowResPaddedOutputFilePath),
      sqlNullable(clip.publishedAssetId),
      sqlNullable(clip.publishedActionOutputFilePath),
      sqlNullable(clip.publishedLowResFilePath),
      sqlNullable(clip.publishedLowResPaddedFilePath),
      sqlNullable(clip.publishedAt),
      clip.status,
      sqlNullable(clip.error),
      clip.createdAt,
      clip.updatedAt
    );
  }
}

function mediaCatalogRowCount(db: DatabaseSync) {
  const row = db.prepare('SELECT COUNT(*) AS count FROM media_video_assets').get() as
    | { count?: number | bigint }
    | undefined;
  return Number(row?.count ?? 0);
}

async function ensureSqliteBootstrap() {
  if (!bootstrapPromise) {
    bootstrapPromise = (async () => {
      try {
        await ensureMediaCatalogRoots();
        const db = getAppDatabase();
        const completed = db.prepare('SELECT value FROM app_state_meta WHERE key = ?').get(SQLITE_BOOTSTRAP_META_KEY) as
          | { value: string }
          | undefined;

        if (completed?.value === 'complete') {
          return;
        }

        if (mediaCatalogRowCount(db) > 0) {
          db.prepare('INSERT OR REPLACE INTO app_state_meta (key, value) VALUES (?, ?)').run(
            SQLITE_BOOTSTRAP_META_KEY,
            'complete'
          );
          return;
        }

        const seed = await readMediaCatalogFromJsonSeed();

        db.exec('BEGIN IMMEDIATE');
        try {
          replaceMediaCatalogInTransaction(db, seed.library);
          db.prepare('INSERT OR REPLACE INTO app_state_meta (key, value) VALUES (?, ?)').run(
            SQLITE_BOOTSTRAP_META_KEY,
            'complete'
          );
          db.exec('COMMIT');
        } catch (error) {
          db.exec('ROLLBACK');
          throw error;
        }
      } catch (error) {
        bootstrapPromise = null;
        throw error;
      }
    })();
  }

  return bootstrapPromise;
}

export async function readMediaCatalog() {
  await libraryWriteQueue;
  await ensureSqliteBootstrap();
  return structuredClone(readMediaCatalogFromSqlite(getAppDatabase()));
}

export async function writeMediaCatalog(library: VideoLibrary) {
  const next = libraryWriteQueue.then(async () => {
    await ensureSqliteBootstrap();
    const db = getAppDatabase();
    db.exec('BEGIN IMMEDIATE');
    try {
      replaceMediaCatalogInTransaction(db, library);
      db.exec('COMMIT');
    } catch (error) {
      db.exec('ROLLBACK');
      throw error;
    }
  });

  libraryWriteQueue = next.then(
    () => undefined,
    () => undefined
  );

  return next;
}

export async function exportMediaCatalogSnapshot(now = new Date()) {
  const library = await readMediaCatalog();
  const exportDir = mediaCatalogExportDir();
  await fs.mkdir(exportDir, { recursive: true });

  const filename = `video-library-${exportTimestamp(now)}.json`;
  const absolutePath = path.join(exportDir, filename);
  await fs.writeFile(absolutePath, `${JSON.stringify(library, null, 2)}\n`, 'utf-8');

  return {
    filePath: `${LIBRARY_EXPORT_DIRNAME}/${filename}`,
    absolutePath,
    counts: {
      videoAssets: library.videoAssets.length,
      moveVideoLinks: library.moveVideoLinks.length,
      derivedClips: library.derivedClips.length
    }
  };
}

export async function mutateMediaCatalog<T>(mutator: (library: VideoLibrary) => Promise<T> | T) {
  const next = libraryWriteQueue.then(async () => {
    await ensureSqliteBootstrap();
    const db = getAppDatabase();
    const library = readMediaCatalogFromSqlite(db);
    const result = await mutator(library);
    db.exec('BEGIN IMMEDIATE');
    try {
      replaceMediaCatalogInTransaction(db, library);
      db.exec('COMMIT');
      return result;
    } catch (error) {
      db.exec('ROLLBACK');
      throw error;
    }
  });

  libraryWriteQueue = next.then(
    () => undefined,
    () => undefined
  );

  return next;
}

export function sortMediaCatalog(library: VideoLibrary) {
  library.videoAssets.sort((left, right) => left.createdAt.localeCompare(right.createdAt));
  library.moveVideoLinks.sort((left, right) => {
    if (left.moveId !== right.moveId) {
      return left.moveId.localeCompare(right.moveId);
    }
    return left.order - right.order;
  });
  library.derivedClips.sort((left, right) => {
    if (left.sourceAssetId !== right.sourceAssetId) {
      return left.sourceAssetId.localeCompare(right.sourceAssetId);
    }
    return left.startMs - right.startMs;
  });
}
