import fs from 'node:fs/promises';
import path from 'node:path';
import { randomUUID } from 'node:crypto';
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
  resolveDataDir,
  resolveMediaRoot,
  resolvePosterRoot,
  resolveSourceRoot
} from './paths';

const LIBRARY_FILENAME = 'video-library.json';

let libraryCache:
  | {
      filePath: string;
      mtimeMs: number;
      library: VideoLibrary;
    }
  | null = null;

let libraryWriteQueue = Promise.resolve();

function nowIso() {
  return new Date().toISOString();
}

function safeDisplayName(filename: string) {
  return path.parse(filename).name.replace(/[_-]+/g, ' ').trim() || path.parse(filename).name;
}

function libraryFilePath() {
  return path.join(resolveDataDir(), LIBRARY_FILENAME);
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

async function readMediaCatalogFromDisk() {
  const filePath = libraryFilePath();

  try {
    const stat = await fs.stat(filePath);
    if (libraryCache && libraryCache.filePath === filePath && libraryCache.mtimeMs === stat.mtimeMs) {
      return structuredClone(libraryCache.library);
    }

    const contents = await fs.readFile(filePath, 'utf-8');
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

    libraryCache = {
      filePath,
      mtimeMs: stat.mtimeMs,
      library
    };

    return structuredClone(library);
  } catch {
    return defaultMediaCatalog();
  }
}

export async function readMediaCatalog() {
  await libraryWriteQueue;
  return readMediaCatalogFromDisk();
}

export async function writeMediaCatalog(library: VideoLibrary) {
  await ensureMediaCatalogRoots();

  const filePath = libraryFilePath();
  const serialized = JSON.stringify(library, null, 2);
  await fs.writeFile(filePath, `${serialized}\n`, 'utf-8');
  const stat = await fs.stat(filePath);
  libraryCache = {
    filePath,
    mtimeMs: stat.mtimeMs,
    library: structuredClone(library)
  };
}

export async function mutateMediaCatalog<T>(mutator: (library: VideoLibrary) => Promise<T> | T) {
  const next = libraryWriteQueue.then(async () => {
    const library = await readMediaCatalogFromDisk();
    const result = await mutator(library);
    await writeMediaCatalog(library);
    return result;
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
