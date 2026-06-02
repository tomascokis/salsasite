import fs from 'node:fs/promises';
import type { Dirent } from 'node:fs';
import path from 'node:path';
import { createHash, randomUUID } from 'node:crypto';
import { spawn } from 'node:child_process';
import type {
  ClipCountMarker,
  ClipCropRect,
  CountOverlayPlacement,
  CountTimingPreset,
  DerivedClip,
  MoveRecord,
  MoveVideoEntry,
  MoveVideoLink,
  MediaHashAlgorithm,
  MediaHashStatus,
  VideoAsset,
  VideoContentType,
  VideoEnvironment,
  VideoLibrary,
  VideoOriginType,
  VideoTiming
} from '$lib/types';
import {
  MOVE_VIDEO_PREFIX,
  SOURCE_VIDEO_PREFIX,
  normalizeManagedVideoPath,
  resolveDataDir,
  resolveManagedVideoAbsolutePath,
  resolveMediaRoot,
  resolvePathInsideRoot,
  resolvePosterRoot,
  resolveSourceRoot
} from './paths';
import { findPosterForVideoFile, queuePosterGeneration } from './posters';
import {
  completedMediaJob,
  completeMediaJob,
  createMediaCleanupJob,
  deleteTemporaryFile,
  failMediaJob,
  getMediaJobById,
  hashFile,
  isMediaJobTargetPending,
  listMediaJobsWithFileActions,
  listQueuedMediaJobs,
  renameManagedVideoFiles,
  recordMediaFileAction,
  restoreTrashedFilesForJob,
  startMediaJob,
  trashManagedVideoFileSet,
  upsertMediaJob,
  writeBufferAndHash,
  writeStreamAndHash,
  type MediaJob,
  type MediaFingerprint
} from './media-manager';
import { recordAction, runInTransaction } from './app-state';
import { getPositionOptions, positionLabelById } from './positions';
import {
  normalizeDateString,
  normalizeOptionalText,
  applyDefaultKeyVideoFlags,
  generatedDerivedClipFileInfo,
  generatedDerivedClipFileMatches,
  obsoleteGeneratedClipFilePaths,
  rekeyClipMoveAssociations,
  normalizeTags,
  sourceSuggestions,
  uploadMonthKey
} from '$lib/video-library-utils';
import { moveDisplayId } from '$lib/move-id';
import { generatedMoveIdStem } from '$lib/move-id-utils.js';
import { publicationStatusFor } from '$lib/content-status';
import { listMoveDrafts } from './move-editor';

const LIBRARY_FILENAME = 'video-library.json';
const VIDEO_EXTENSIONS = new Set(['.mp4', '.m4v', '.mov']);
const CLIP_OUTPUT_EXTENSION = '.mp4';
const FULL_QUALITY_CRF = '18';
const LOW_QUALITY_CRF = '29';
let renderWorkerRunning = false;
let sourceHashWorkerRunning = false;

let libraryCache:
  | {
      filePath: string;
      mtimeMs: number;
      library: VideoLibrary;
    }
  | null = null;

let libraryWriteQueue = Promise.resolve();

function defaultLibrary(): VideoLibrary {
  return {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  };
}

function libraryFilePath() {
  return path.join(resolveDataDir(), LIBRARY_FILENAME);
}

function nowIso() {
  return new Date().toISOString();
}

function recordVideoAuditAction(input: {
  type: string;
  label: string;
  entityType: string;
  entityId: string;
  before: unknown;
  after: unknown;
}) {
  runInTransaction((db) => {
    recordAction(db, input);
  });
}

function sourceAssetAuditSnapshot(asset: VideoAsset | null | undefined) {
  if (!asset) {
    return null;
  }
  return {
    id: asset.id,
    filePath: asset.filePath,
    displayName: asset.displayName,
    originalFilename: asset.originalFilename,
    dancers: [...asset.dancers],
    timing: asset.timing,
    contentType: asset.contentType,
    environment: asset.environment,
    originType: asset.originType,
    sourceUrl: asset.sourceUrl,
    recordDate: asset.recordDate,
    classWorkshop: asset.classWorkshop,
    tags: [...asset.tags],
    notes: asset.notes,
    contentHash: asset.contentHash,
    contentSizeBytes: asset.contentSizeBytes,
    hashStatus: asset.hashStatus,
    createdAt: asset.createdAt
  };
}

function derivedClipAuditSnapshot(clip: DerivedClip | null | undefined) {
  if (!clip) {
    return null;
  }
  return {
    id: clip.id,
    sourceAssetId: clip.sourceAssetId,
    moveId: clip.moveId,
    moveDisplayId: clip.moveDisplayId ?? null,
    isKeyVideo: clip.isKeyVideo,
    label: clip.label,
    descriptorLabel: clip.descriptorLabel,
    startPositionId: clip.startPositionId,
    endPositionId: clip.endPositionId,
    timingGroupId: clip.timingGroupId,
    manuallyNamed: clip.manuallyNamed,
    startMs: clip.startMs,
    endMs: clip.endMs,
    actionStartMs: clip.actionStartMs,
    actionEndMs: clip.actionEndMs,
    cropRect: clip.cropRect,
    countMarkerCount: clip.countMarkers.length,
    countOverlayPlacement: clip.countOverlayPlacement,
    countTimingPreset: clip.countTimingPreset,
    outputAssetId: clip.outputAssetId,
    actionOutputFilePath: clip.actionOutputFilePath,
    lowResOutputFilePath: clip.lowResOutputFilePath,
    lowResPaddedOutputFilePath: clip.lowResPaddedOutputFilePath,
    publishedAssetId: clip.publishedAssetId,
    publishedAt: clip.publishedAt,
    status: clip.status,
    updatedAt: clip.updatedAt
  };
}

function moveVideoLinkAuditSnapshot(link: MoveVideoLink) {
  return {
    id: link.id,
    moveId: link.moveId,
    assetId: link.assetId,
    order: link.order,
    createdAt: link.createdAt
  };
}

function safeDisplayName(filename: string) {
  return path.parse(filename).name.replace(/[_-]+/g, ' ').trim() || path.parse(filename).name;
}

function sanitizeFilenamePart(value: string) {
  return value
    .trim()
    .replace(/[\/\\?%*:|"<>]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

function timingLabel(timing: VideoTiming) {
  if (timing === 'on1') return 'On1';
  if (timing === 'on2') return 'On2';
  return 'Other';
}

function contentTypeLabel(contentType: VideoContentType) {
  if (contentType === 'music') return 'Music';
  if (contentType === 'counts') return 'Counts';
  return 'Other';
}

function environmentLabel(environment: VideoEnvironment) {
  if (environment === 'social') return 'Social';
  return 'Class';
}

function generatedClipLabel(sourceAsset: VideoAsset, moveId: string, index: number) {
  const dancers = sourceAsset.dancers.length ? sourceAsset.dancers.join(' and ') : sourceAsset.displayName;
  const date = sourceAsset.recordDate ? new Date(sourceAsset.recordDate) : null;
  const month =
    date && !Number.isNaN(date.getTime())
      ? new Intl.DateTimeFormat('en-AU', { month: 'short' }).format(date)
      : null;
  const context = sourceAsset.classWorkshop?.trim() || `${environmentLabel(sourceAsset.environment).toLowerCase()} clip`;
  const suffix = index > 0 ? ` #${index + 1}` : '';
  return [dancers, month, context].filter(Boolean).join(' ') + suffix || `${moveId} clip${suffix}`;
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

function cropRectsEqual(left: ClipCropRect | null, right: ClipCropRect | null) {
  if (!left || !right) {
    return left === right;
  }

  return left.x === right.x && left.y === right.y && left.width === right.width && left.height === right.height;
}

function countMarkersEqual(left: ClipCountMarker[], right: ClipCountMarker[]) {
  if (left.length !== right.length) {
    return false;
  }

  return left.every((marker, index) => {
    const other = right[index];
    return other && marker.count === other.count && marker.ms === other.ms && marker.clear === other.clear;
  });
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

async function ensureRoots() {
  await Promise.all([
    fs.mkdir(resolveDataDir(), { recursive: true }),
    fs.mkdir(resolveMediaRoot(), { recursive: true }),
    fs.mkdir(resolveSourceRoot(), { recursive: true }),
    fs.mkdir(resolvePosterRoot(), { recursive: true })
  ]);
}

async function readLibraryFromDisk() {
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
    return defaultLibrary();
  }
}

async function writeLibraryToDisk(library: VideoLibrary) {
  await ensureRoots();

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

async function mutateLibrary<T>(mutator: (library: VideoLibrary) => Promise<T> | T) {
  const next = libraryWriteQueue.then(async () => {
    const library = await readLibraryFromDisk();
    const result = await mutator(library);
    await writeLibraryToDisk(library);
    return result;
  });

  libraryWriteQueue = next.then(
    () => undefined,
    () => undefined
  );

  return next;
}

async function walkVideoFiles(root: string, prefix: string) {
  const result: Array<{ relativePath: string; originalFilename: string; createdAt: string }> = [];

  async function visit(directory: string, relativeDir = ''): Promise<void> {
    let entries: Dirent[];
    try {
      entries = await fs.readdir(directory, { withFileTypes: true });
    } catch {
      return;
    }

    for (const entry of entries) {
      if (entry.name.startsWith('.')) {
        continue;
      }

      const absolutePath = path.join(directory, entry.name);
      const nextRelativeDir = relativeDir ? path.posix.join(relativeDir, entry.name) : entry.name;

      if (entry.isDirectory()) {
        await visit(absolutePath, nextRelativeDir);
        continue;
      }

      const extension = path.extname(entry.name).toLowerCase();
      if (!VIDEO_EXTENSIONS.has(extension)) {
        continue;
      }

      const stat = await fs.stat(absolutePath);
      result.push({
        relativePath: path.posix.join(prefix, nextRelativeDir),
        originalFilename: entry.name,
        createdAt: stat.birthtime?.toISOString?.() ?? stat.mtime.toISOString()
      });
    }
  }

  await visit(root);
  result.sort((left, right) => left.relativePath.localeCompare(right.relativePath, undefined, { numeric: true }));
  return result;
}

function extractMoveIdFromFilename(filename: string) {
  const match = /^([A-Za-z0-9]+)/.exec(filename);
  return match ? match[1].toUpperCase() : null;
}

function legacyAssetId(filePath: string) {
  return `asset-${createHash('sha1').update(filePath).digest('hex').slice(0, 12)}`;
}

function ensureMoveLink(library: VideoLibrary, moveId: string, assetId: string) {
  const existing = library.moveVideoLinks.find((entry) => entry.moveId === moveId && entry.assetId === assetId);
  if (existing) {
    return existing;
  }

  const order = Math.max(
    -1,
    ...library.moveVideoLinks.filter((entry) => entry.moveId === moveId).map((entry) => entry.order)
  ) + 1;

  const link: MoveVideoLink = {
    id: randomUUID(),
    moveId,
    assetId,
    order,
    createdAt: nowIso()
  };

  library.moveVideoLinks.push(link);
  return link;
}

function publishClipToMove(library: VideoLibrary, clip: DerivedClip, publishedAt: string) {
  if (clip.status !== 'ready' || !clip.outputAssetId) {
    throw new Error('Only rendered clips can be published.');
  }

  const outputAsset = library.videoAssets.find((asset) => asset.id === clip.outputAssetId && asset.kind === 'move');
  const sourceAsset = library.videoAssets.find((asset) => asset.id === clip.sourceAssetId && asset.kind === 'source');
  if (!outputAsset || !sourceAsset) {
    throw new Error('Rendered clip asset not found.');
  }

  outputAsset.displayName = clip.label?.trim() || sourceAsset.displayName;
  outputAsset.dancers = [...sourceAsset.dancers];
  outputAsset.timing = sourceAsset.timing;
  outputAsset.contentType = sourceAsset.contentType;
  outputAsset.environment = sourceAsset.environment;
  outputAsset.originType = sourceAsset.originType;
  outputAsset.sourceUrl = sourceAsset.sourceUrl;
  outputAsset.recordDate = sourceAsset.recordDate;
  outputAsset.classWorkshop = sourceAsset.classWorkshop;
  outputAsset.tags = [...sourceAsset.tags];
  outputAsset.notes = sourceAsset.notes;

  if (clip.publishedAssetId && clip.publishedAssetId !== clip.outputAssetId) {
    library.moveVideoLinks = library.moveVideoLinks.filter((link) => link.assetId !== clip.publishedAssetId);
  }

  ensureMoveLink(library, clip.moveId, outputAsset.id);
  clip.publishedAssetId = outputAsset.id;
  clip.publishedActionOutputFilePath = clip.actionOutputFilePath;
  clip.publishedLowResFilePath = clip.lowResOutputFilePath;
  clip.publishedLowResPaddedFilePath = clip.lowResPaddedOutputFilePath;
  clip.publishedAt = publishedAt;
  clip.updatedAt = publishedAt;
  return clip;
}

function derivedClipVariantPaths(library: VideoLibrary) {
  const paths = new Set<string>();
  for (const clip of library.derivedClips) {
    [
      clip.actionOutputFilePath,
      clip.lowResOutputFilePath,
      clip.lowResPaddedOutputFilePath,
      clip.publishedActionOutputFilePath,
      clip.publishedLowResFilePath,
      clip.publishedLowResPaddedFilePath
    ].forEach((filePath) => {
      if (filePath) {
        paths.add(normalizeManagedVideoPath(filePath));
      }
    });
  }
  return paths;
}

async function pruneMissingDerivedVariantPaths(library: VideoLibrary) {
  let changed = false;
  const variantKeys = [
    'actionOutputFilePath',
    'lowResOutputFilePath',
    'lowResPaddedOutputFilePath',
    'publishedActionOutputFilePath',
    'publishedLowResFilePath',
    'publishedLowResPaddedFilePath'
  ] as const;

  for (const clip of library.derivedClips) {
    for (const key of variantKeys) {
      const filePath = clip[key];
      if (!filePath) {
        continue;
      }

      if (!(await managedVideoFileExists(filePath))) {
        clip[key] = null;
        changed = true;
      }
    }
  }

  return changed;
}

async function managedVideoFileExists(filePath: string) {
  try {
    await fs.access(resolveManagedVideoAbsolutePath(filePath));
    return true;
  } catch {
    return false;
  }
}

function derivedClipOutputAssetIds(library: VideoLibrary) {
  return new Set(
    library.derivedClips
      .flatMap((clip) => [clip.outputAssetId, clip.publishedAssetId])
      .filter((id): id is string => Boolean(id))
  );
}

function generatedClipOwnershipEntries(library: VideoLibrary) {
  return library.derivedClips.map((clip) => {
    const sourceAsset = library.videoAssets.find((asset) => asset.id === clip.sourceAssetId && asset.kind === 'source');
    return {
      id: clip.id,
      moveId: clip.moveId,
      moveDisplayId: clip.moveDisplayId ?? clip.moveId,
      sourceDisplayName: sourceAsset?.displayName ?? ''
    };
  });
}

function isGeneratedDerivedClipPath(library: VideoLibrary, filePath: string) {
  return generatedDerivedClipFileMatches(
    normalizeManagedVideoPath(filePath),
    generatedClipOwnershipEntries(library)
  );
}

function isGeneratedDerivedClipVariantPath(filePath: string) {
  const info = generatedDerivedClipFileInfo(normalizeManagedVideoPath(filePath));
  return info?.variant === 'low' || info?.variant === 'padded-low';
}

function isGeneratedDerivedClipPathFromKnownSource(library: VideoLibrary, filePath: string) {
  const info = generatedDerivedClipFileInfo(normalizeManagedVideoPath(filePath));
  if (!info) {
    return false;
  }

  return library.videoAssets.some(
    (asset) => asset.kind === 'source' && sanitizeFilenamePart(asset.displayName) === info.sourceDisplayName
  );
}

async function bootstrapLegacyMoveAssets(library: VideoLibrary, moves: MoveRecord[]) {
  const moveIds = new Set(moves.map((move) => move.id.toUpperCase()));
  const variantPaths = derivedClipVariantPaths(library);
  const outputAssetIds = derivedClipOutputAssetIds(library);
  const linkedAssetIds = new Set(library.moveVideoLinks.map((link) => link.assetId));
  const variantAssetIds = new Set(
    library.videoAssets
      .filter((asset) => asset.kind === 'move' && variantPaths.has(normalizeManagedVideoPath(asset.filePath)))
      .map((asset) => asset.id)
  );
  const staleGeneratedAssets = library.videoAssets.filter(
    (asset) => {
      if (asset.kind !== 'move' || outputAssetIds.has(asset.id)) {
        return false;
      }

      const normalizedPath = normalizeManagedVideoPath(asset.filePath);
      if (variantPaths.has(normalizedPath)) {
        return false;
      }

      const info = generatedDerivedClipFileInfo(normalizedPath);
      if (!info) {
        return false;
      }

      if (info.variant === 'low' || info.variant === 'padded-low') {
        return true;
      }

      return (
        linkedAssetIds.has(asset.id) &&
        (isGeneratedDerivedClipPath(library, normalizedPath) ||
          isGeneratedDerivedClipPathFromKnownSource(library, normalizedPath))
      );
    }
  );
  const staleGeneratedAssetIds = new Set(staleGeneratedAssets.map((asset) => asset.id));
  const assetIdsToUnlink = new Set([...variantAssetIds, ...staleGeneratedAssetIds]);
  const linkCountBeforeVariantCleanup = library.moveVideoLinks.length;
  library.moveVideoLinks = library.moveVideoLinks.filter((link) => !assetIdsToUnlink.has(link.assetId));
  library.videoAssets = library.videoAssets.filter((asset) => !staleGeneratedAssetIds.has(asset.id));
  const files = await walkVideoFiles(resolveMediaRoot(), MOVE_VIDEO_PREFIX);
  let changed =
    library.moveVideoLinks.length !== linkCountBeforeVariantCleanup ||
    staleGeneratedAssets.length > 0;

  if (staleGeneratedAssets.length) {
    const cleanupJob = createMediaCleanupJob({
      targetType: 'legacyGeneratedAssets',
      targetId: 'bootstrap',
      payload: {
        assetIds: staleGeneratedAssets.map((asset) => asset.id),
        filePaths: staleGeneratedAssets.map((asset) => asset.filePath)
      }
    });
    startMediaJob(cleanupJob.id);
    try {
      await trashManagedVideoFileSet({
        jobId: cleanupJob.id,
        filePaths: staleGeneratedAssets.map((asset) => asset.filePath),
        actionType: 'cleanup-generated',
        metadata: { cleanupReason: 'legacy-generated-bootstrap' }
      });
      completeMediaJob(cleanupJob.id);
    } catch (error) {
      failMediaJob(cleanupJob.id, error);
      throw error;
    }
  }

  for (const file of files) {
    const normalizedPath = normalizeManagedVideoPath(file.relativePath);
    if (
      variantPaths.has(normalizedPath) ||
      isGeneratedDerivedClipPath(library, normalizedPath) ||
      isGeneratedDerivedClipVariantPath(normalizedPath)
    ) {
      continue;
    }

    let asset = library.videoAssets.find((entry) => normalizeManagedVideoPath(entry.filePath) === normalizedPath);

    if (!asset) {
      asset = {
        id: legacyAssetId(normalizedPath),
        kind: 'move',
        filePath: normalizedPath,
        displayName: safeDisplayName(file.originalFilename),
        originalFilename: file.originalFilename,
        dancers: [],
        timing: 'other',
        contentType: 'other',
        environment: 'class',
        originType: 'self-recorded',
        sourceUrl: null,
        recordDate: null,
        classWorkshop: null,
        tags: [],
        notes: null,
        contentHash: null,
        contentHashAlgorithm: null,
        contentSizeBytes: null,
        hashStatus: 'pending',
        createdAt: file.createdAt
      };
      library.videoAssets.push(asset);
      changed = true;
    }

    const moveId = extractMoveIdFromFilename(file.originalFilename);
    if (moveId && moveIds.has(moveId)) {
      const existing = library.moveVideoLinks.find((entry) => entry.moveId === moveId && entry.assetId === asset.id);
      if (!existing) {
        ensureMoveLink(library, moveId, asset.id);
        changed = true;
      }
    }
  }

  return changed;
}

export async function getVideoLibrary(moves?: MoveRecord[]) {
  const library = await readLibraryFromDisk();

  if (moves) {
    let changed = await pruneMissingDerivedVariantPaths(library);
    changed = (await bootstrapLegacyMoveAssets(library, moves)) || changed;
    changed = (await relinkOrphanedGeneratedDraftMoveIds(library, moves)) || changed;
    if (changed) {
      await writeLibraryToDisk(library);
      return structuredClone(library);
    }
  }

  return library;
}

async function relinkOrphanedGeneratedDraftMoveIds(library: VideoLibrary, moves: MoveRecord[]) {
  const publishedMoveIds = new Set(moves.map((move) => move.id.toUpperCase()));
  const activeDraftIds = new Set((await listMoveDrafts()).map((draft) => draft.move.id.toUpperCase()));
  const aliasCandidates = new Map<string, MoveRecord | null>();

  for (const move of moves) {
    const generatedDraftId = generatedMoveIdStem(move.name ?? '');
    const normalizedMoveId = move.id.toUpperCase();
    if (!generatedDraftId || generatedDraftId === normalizedMoveId || publishedMoveIds.has(generatedDraftId) || activeDraftIds.has(generatedDraftId)) {
      continue;
    }

    aliasCandidates.set(generatedDraftId, aliasCandidates.has(generatedDraftId) ? null : move);
  }

  let changed = false;
  for (const [draftMoveId, move] of aliasCandidates.entries()) {
    if (!move) {
      continue;
    }

    const hasOrphanedMedia =
      library.derivedClips.some((clip) => String(clip.moveId ?? '').trim().toUpperCase() === draftMoveId) ||
      library.moveVideoLinks.some((link) => String(link.moveId ?? '').trim().toUpperCase() === draftMoveId);
    if (!hasOrphanedMedia) {
      continue;
    }

    const relinked = rekeyClipMoveAssociations(library, draftMoveId, move.id, moveDisplayId(move));
    if (!relinked.changed) {
      continue;
    }

    library.derivedClips = relinked.derivedClips as DerivedClip[];
    library.moveVideoLinks = relinked.moveVideoLinks as MoveVideoLink[];
    await syncDerivedClipDisplayIdForMoveInLibrary(library, move.id, moveDisplayId(move));
    sortLibrary(library);
    changed = true;
  }

  return changed;
}

export async function getResolvedMoveVideos(moveId: string, moves: MoveRecord[]) {
  const library = await getVideoLibrary(moves);
  const positionLabels = positionLabelById(await getPositionOptions(moves));
  const assetById = new Map(library.videoAssets.map((asset) => [asset.id, asset]));
  const clipByOutputAssetId = new Map<string, DerivedClip>();
  library.derivedClips.forEach((clip) => {
    if (clip.outputAssetId) {
      clipByOutputAssetId.set(clip.outputAssetId, clip);
    }
    if (clip.publishedAssetId) {
      clipByOutputAssetId.set(clip.publishedAssetId, clip);
    }
  });
  const moveLinks = library.moveVideoLinks
    .filter((entry) => entry.moveId === moveId)
    .sort((left, right) => left.order - right.order);

  const result: MoveVideoEntry[] = [];

  for (const link of moveLinks) {
    const asset = assetById.get(link.assetId);
    if (!asset || asset.kind === 'source') {
      continue;
    }

    const clip = clipByOutputAssetId.get(asset.id) ?? null;
    const sourceAsset = clip ? assetById.get(clip.sourceAssetId) ?? null : null;
    const posterFile = await findPosterForVideoFile(asset.filePath);

    result.push({
      assetId: asset.id,
      filePath: asset.filePath,
      actionFilePath: clip?.publishedAssetId === asset.id ? clip.publishedActionOutputFilePath : null,
      lowResFilePath: clip?.publishedAssetId === asset.id ? clip.publishedLowResFilePath : null,
      lowResPaddedFilePath: clip?.publishedAssetId === asset.id ? clip.publishedLowResPaddedFilePath : null,
      displayName: asset.displayName,
      posterFile,
      isDerived: Boolean(clip),
      kind: asset.kind,
      dancers: asset.dancers,
      timing: asset.timing,
      contentType: asset.contentType,
      environment: asset.environment,
      recordDate: asset.recordDate,
      classWorkshop: asset.classWorkshop,
      tags: asset.tags,
      notes: asset.notes,
      sourceAssetId: sourceAsset?.id ?? null,
      sourceDisplayName: sourceAsset?.displayName ?? null,
      sourceOriginalFilename: sourceAsset?.originalFilename ?? null,
      sourceDancers: sourceAsset?.dancers ?? [],
      sourceRecordDate: sourceAsset?.recordDate ?? null,
      sourceClassWorkshop: sourceAsset?.classWorkshop ?? null,
      sourceTags: sourceAsset?.tags ?? [],
      sourceNotes: sourceAsset?.notes ?? null,
      timingLabel: timingLabel(sourceAsset?.timing ?? asset.timing),
      contentTypeLabel: contentTypeLabel(sourceAsset?.contentType ?? asset.contentType),
      environmentLabel: environmentLabel(sourceAsset?.environment ?? asset.environment),
      clipId: clip?.id ?? null,
      clipStartMs: clip?.publishedAssetId === asset.id ? clip.startMs : null,
      clipActionStartMs: clip?.publishedAssetId === asset.id ? clip.actionStartMs : null,
      isKeyVideo: clip?.isKeyVideo ?? false,
      countMarkers: clip?.publishedAssetId === asset.id ? clip.countMarkers : [],
      countOverlayPlacement: clip?.countOverlayPlacement ?? 'top-left',
      moveId,
      moveDisplayId: clip?.moveDisplayId ?? moveId,
      descriptorLabel: clip?.descriptorLabel ?? null,
      startPositionId: clip?.startPositionId ?? null,
      startPositionLabel: clip?.startPositionId ? positionLabels.get(clip.startPositionId) ?? null : null,
      endPositionId: clip?.endPositionId ?? null,
      endPositionLabel: clip?.endPositionId ? positionLabels.get(clip.endPositionId) ?? null : null,
      timingGroupId: clip?.timingGroupId ?? null
    });
  }

  return result;
}

export async function buildResolvedMoveVideoIndex(moves: MoveRecord[]) {
  const metadataIndex = await buildResolvedMoveVideoMetadataIndex(moves);
  return new Map([...metadataIndex.entries()].map(([moveId, metadata]) => [moveId, metadata.files]));
}

export async function buildResolvedMoveVideoMetadataIndex(moves: MoveRecord[]) {
  const library = await getVideoLibrary(moves);
  const byMoveId = new Map<string, { files: string[]; previewFile: string | null }>();
  const clipByAssetId = new Map<string, DerivedClip>();

  library.derivedClips.forEach((clip) => {
    if (clip.outputAssetId) {
      clipByAssetId.set(clip.outputAssetId, clip);
    }
    if (clip.publishedAssetId) {
      clipByAssetId.set(clip.publishedAssetId, clip);
    }
  });

  for (const move of moves) {
    const links = library.moveVideoLinks
      .filter((entry) => entry.moveId === move.id)
      .sort((left, right) => left.order - right.order);
    const assetFiles = links
      .map((entry) => library.videoAssets.find((asset) => asset.id === entry.assetId)?.filePath ?? null)
      .filter((_, index) => {
        const asset = library.videoAssets.find((entry) => entry.id === links[index]?.assetId);
        return asset?.kind !== 'source';
      })
      .filter((value): value is string => Boolean(value))
      .map((value) => normalizeManagedVideoPath(value));

    const previewFile =
      links
        .map((entry) => {
          const asset = library.videoAssets.find((candidate) => candidate.id === entry.assetId);
          if (!asset || asset.kind === 'source') {
            return null;
          }

          const clip = clipByAssetId.get(asset.id);
          return normalizeManagedVideoPath(clip?.publishedLowResFilePath ?? asset.filePath);
        })
        .find((value): value is string => Boolean(value)) ?? null;

    byMoveId.set(move.id.toUpperCase(), { files: assetFiles, previewFile });
  }

  return byMoveId;
}

function normalizeDancers(dancers: string[] | string) {
  const values = Array.isArray(dancers) ? dancers : dancers.split(',');
  return values.map((value) => value.trim()).filter(Boolean);
}

function clipSuffixFromPath(filePath: string | null, clipId: string) {
  if (!filePath) {
    return '';
  }

  const parsed = path.parse(filePath);
  const clipPrefix = clipId.slice(0, 8);
  const marker = ` ${clipPrefix}`;
  const markerIndex = parsed.name.lastIndexOf(marker);
  if (markerIndex < 0) {
    return '';
  }

  const suffix = parsed.name.slice(markerIndex + marker.length).trim();
  return suffix;
}

async function syncDerivedClipDisplayIdForMoveInLibrary(
  library: VideoLibrary,
  normalizedMoveId: string,
  normalizedDisplayId: string
) {
  const pendingRenames = new Map<string, string>();

  for (const clip of library.derivedClips.filter((entry) => entry.moveId === normalizedMoveId)) {
    const sourceAsset = library.videoAssets.find((asset) => asset.id === clip.sourceAssetId && asset.kind === 'source');
    if (!sourceAsset) {
      clip.moveDisplayId = normalizedDisplayId;
      continue;
    }

    const outputPath = clip.outputAssetId
      ? path.posix.join(
          MOVE_VIDEO_PREFIX,
          clipOutputRelativePath(
            normalizedDisplayId,
            sourceAsset.displayName,
            clip.id,
            clipSuffixFromPath(clip.outputAssetId ? library.videoAssets.find((asset) => asset.id === clip.outputAssetId)?.filePath ?? null : null, clip.id)
          )
        )
      : null;
    const lowResPath = clip.lowResOutputFilePath
      ? path.posix.join(
          MOVE_VIDEO_PREFIX,
          clipOutputRelativePath(normalizedDisplayId, sourceAsset.displayName, clip.id, clipSuffixFromPath(clip.lowResOutputFilePath, clip.id))
        )
      : null;
    const actionPath = clip.actionOutputFilePath
      ? path.posix.join(
          MOVE_VIDEO_PREFIX,
          clipOutputRelativePath(normalizedDisplayId, sourceAsset.displayName, clip.id, clipSuffixFromPath(clip.actionOutputFilePath, clip.id))
        )
      : null;
    const lowResPaddedPath = clip.lowResPaddedOutputFilePath
      ? path.posix.join(
          MOVE_VIDEO_PREFIX,
          clipOutputRelativePath(
            normalizedDisplayId,
            sourceAsset.displayName,
            clip.id,
            clipSuffixFromPath(clip.lowResPaddedOutputFilePath, clip.id)
          )
        )
      : null;

    const queueRename = (fromPath: string | null, toPath: string | null) => {
      if (!fromPath || !toPath || fromPath === toPath) return;
      pendingRenames.set(fromPath, toPath);
    };

    const outputAsset = clip.outputAssetId ? library.videoAssets.find((asset) => asset.id === clip.outputAssetId) ?? null : null;
    const publishedAsset = clip.publishedAssetId ? library.videoAssets.find((asset) => asset.id === clip.publishedAssetId) ?? null : null;

    queueRename(outputAsset?.filePath ?? null, outputPath);
    queueRename(publishedAsset?.filePath ?? null, outputPath);
    queueRename(clip.actionOutputFilePath, actionPath);
    queueRename(clip.publishedActionOutputFilePath, actionPath);
    queueRename(clip.lowResOutputFilePath, lowResPath);
    queueRename(clip.lowResPaddedOutputFilePath, lowResPaddedPath);
    queueRename(clip.publishedLowResFilePath, lowResPath);
    queueRename(clip.publishedLowResPaddedFilePath, lowResPaddedPath);

    if (outputAsset && outputPath) {
      outputAsset.filePath = outputPath;
      outputAsset.originalFilename = path.basename(outputPath);
    }
    if (publishedAsset && outputPath) {
      publishedAsset.filePath = outputPath;
      publishedAsset.originalFilename = path.basename(outputPath);
    }

    clip.moveDisplayId = normalizedDisplayId;
    if (actionPath) {
      clip.actionOutputFilePath = actionPath;
      clip.publishedActionOutputFilePath = actionPath;
    }
    if (lowResPath) {
      clip.lowResOutputFilePath = lowResPath;
      clip.publishedLowResFilePath = lowResPath;
    }
    if (lowResPaddedPath) {
      clip.lowResPaddedOutputFilePath = lowResPaddedPath;
      clip.publishedLowResPaddedFilePath = lowResPaddedPath;
    }
  }

  if (pendingRenames.size) {
    const renameJob = createMediaCleanupJob({
      targetType: 'move',
      targetId: normalizedMoveId,
      payload: {
        nextMoveDisplayId: normalizedDisplayId,
        renames: [...pendingRenames.entries()].map(([fromPath, toPath]) => ({ fromPath, toPath }))
      }
    });
    startMediaJob(renameJob.id);
    try {
      for (const [fromPath, toPath] of pendingRenames.entries()) {
        await renameManagedVideoFiles({
          jobId: renameJob.id,
          fromPath,
          toPath,
          metadata: {
            moveId: normalizedMoveId,
            nextMoveDisplayId: normalizedDisplayId
          }
        });
      }
      completeMediaJob(renameJob.id);
    } catch (error) {
      failMediaJob(renameJob.id, error);
      throw error;
    }
  }
}

export async function syncDerivedClipDisplayIdForMove(moveId: string, moveDisplayId: string) {
  const normalizedMoveId = moveId.trim().toUpperCase();
  const normalizedDisplayId = moveDisplayId.trim().toUpperCase();
  if (!normalizedMoveId || !normalizedDisplayId) {
    return;
  }

  await mutateLibrary(async (library) => {
    await syncDerivedClipDisplayIdForMoveInLibrary(library, normalizedMoveId, normalizedDisplayId);
    sortLibrary(library);
  });
}

export async function relinkDerivedClipsForPublishedMove(previousMoveId: string, nextMoveId: string, nextMoveDisplayId: string) {
  const normalizedPreviousMoveId = previousMoveId.trim().toUpperCase();
  const normalizedNextMoveId = nextMoveId.trim().toUpperCase();
  const normalizedNextMoveDisplayId = nextMoveDisplayId.trim().toUpperCase();
  if (!normalizedPreviousMoveId || !normalizedNextMoveId || !normalizedNextMoveDisplayId) {
    return;
  }

  await mutateLibrary(async (library) => {
    const relinked = rekeyClipMoveAssociations(
      {
        derivedClips: library.derivedClips,
        moveVideoLinks: library.moveVideoLinks
      },
      normalizedPreviousMoveId,
      normalizedNextMoveId,
      normalizedNextMoveDisplayId
    );

    if (relinked.changed) {
      library.derivedClips = relinked.derivedClips as DerivedClip[];
      library.moveVideoLinks = relinked.moveVideoLinks as MoveVideoLink[];
    }

    await syncDerivedClipDisplayIdForMoveInLibrary(library, normalizedNextMoveId, normalizedNextMoveDisplayId);
    sortLibrary(library);
  });
}

function sortLibrary(library: VideoLibrary) {
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

function uniquePathForDirectory(fileName: string, relativeDirectory: string, takenPaths: Set<string>) {
  const parsed = path.parse(fileName);
  const safeName = sanitizeFilenamePart(parsed.name) || 'video';
  const safeExtension = parsed.ext || '.mp4';
  let candidate = path.posix.join(relativeDirectory, `${safeName}${safeExtension}`);
  let suffix = 2;

  while (takenPaths.has(candidate)) {
    candidate = path.posix.join(relativeDirectory, `${safeName} ${suffix}${safeExtension}`);
    suffix += 1;
  }

  takenPaths.add(candidate);
  return candidate;
}

export async function createSourceAsset(input: {
  originalFilename: string;
  displayName: string;
  dancers: string[] | string;
  timing: VideoTiming;
  contentType: VideoContentType;
  environment: VideoEnvironment;
  originType?: VideoOriginType | null;
  sourceUrl?: string | null;
  recordDate?: string | null;
  classWorkshop?: string | null;
  tags?: string[] | string | null;
  notes: string | null;
  fileBuffer?: Buffer;
  fileStream?: ReadableStream<Uint8Array>;
}) {
  const result = await mutateLibrary(async (library) => {
    await ensureRoots();

    const takenPaths = new Set(
      library.videoAssets
        .filter((asset) => asset.kind === 'source')
        .map((asset) => normalizeManagedVideoPath(asset.filePath).replace(new RegExp(`^${SOURCE_VIDEO_PREFIX}/`), ''))
    );
    const relativeFilePath = uniquePathForDirectory(input.originalFilename, '', takenPaths);
    const absolutePath = resolvePathInsideRoot(resolveSourceRoot(), relativeFilePath);
    let fingerprint: MediaFingerprint;
    if (input.fileStream) {
      fingerprint = await writeStreamAndHash({
        stream: input.fileStream,
        absolutePath
      });
    } else if (input.fileBuffer) {
      fingerprint = await writeBufferAndHash({
        buffer: input.fileBuffer,
        absolutePath
      });
    } else {
      throw new Error('A source video file is required.');
    }

    const duplicateAsset = library.videoAssets.find(
      (asset) =>
        asset.kind === 'source' &&
        asset.contentHash === fingerprint.contentHash &&
        asset.contentSizeBytes === fingerprint.contentSizeBytes
    );
    if (duplicateAsset) {
      const cleanupJob = createMediaCleanupJob({
        targetType: 'videoAsset',
        targetId: duplicateAsset.id,
        payload: {
          duplicateOfAssetId: duplicateAsset.id,
          temporaryPath: relativeFilePath
        }
      });
      startMediaJob(cleanupJob.id);
      try {
        await deleteTemporaryFile({
          jobId: cleanupJob.id,
          absolutePath,
          filePath: path.posix.join(SOURCE_VIDEO_PREFIX, relativeFilePath),
          metadata: {
            duplicateOfAssetId: duplicateAsset.id,
            contentHash: fingerprint.contentHash,
            contentSizeBytes: fingerprint.contentSizeBytes
          }
        });
        completeMediaJob(cleanupJob.id);
      } catch (error) {
        failMediaJob(cleanupJob.id, error);
        throw error;
      }
      return {
        asset: duplicateAsset,
        reusedExisting: true
      };
    }

    const assetId = randomUUID();

    const asset: VideoAsset = {
      id: assetId,
      kind: 'source',
      filePath: path.posix.join(SOURCE_VIDEO_PREFIX, relativeFilePath),
      displayName: input.displayName.trim() || safeDisplayName(input.originalFilename),
      originalFilename: input.originalFilename,
      dancers: normalizeDancers(input.dancers),
      timing: input.timing,
      contentType: input.contentType,
      environment: input.environment,
      originType: input.originType === 'download' ? 'download' : 'self-recorded',
      sourceUrl: input.originType === 'download' ? normalizeOptionalText(input.sourceUrl) : null,
      recordDate: normalizeDateString(input.recordDate),
      classWorkshop: normalizeOptionalText(input.classWorkshop),
      tags: normalizeTags(input.tags),
      notes: input.notes?.trim() || null,
      contentHash: fingerprint.contentHash,
      contentHashAlgorithm: fingerprint.contentHashAlgorithm,
      contentSizeBytes: fingerprint.contentSizeBytes,
      hashStatus: 'ready',
      createdAt: nowIso()
    };

    library.videoAssets.push(asset);
    sortLibrary(library);

    return {
      asset,
      reusedExisting: false,
      fingerprint
    };
  });

  if (!result.reusedExisting && result.fingerprint) {
    const fingerprint = result.fingerprint;
    const hashJob = completedMediaJob({
      type: 'source.hash',
      targetType: 'videoAsset',
      targetId: result.asset.id,
      idempotencyKey: `source.hash:${result.asset.id}:${fingerprint.contentHash}`,
      payload: {
        filePath: result.asset.filePath,
        contentHash: fingerprint.contentHash,
        contentSizeBytes: fingerprint.contentSizeBytes
      }
    });
    recordMediaFileAction({
      jobId: hashJob.id,
      actionType: 'write-source',
      status: 'succeeded',
      filePath: result.asset.filePath,
      metadata: {
        contentHash: fingerprint.contentHash,
        contentSizeBytes: fingerprint.contentSizeBytes
      }
    });
    recordVideoAuditAction({
      type: 'media.source.create',
      label: `Uploaded source video: ${result.asset.displayName}`,
      entityType: 'media:source',
      entityId: result.asset.id,
      before: null,
      after: {
        asset: sourceAssetAuditSnapshot(result.asset),
        mediaJobIds: [hashJob.id]
      }
    });
  }

  return {
    asset: result.asset,
    reusedExisting: result.reusedExisting
  };
}

export async function updateSourceAsset(input: {
  assetId: string;
  displayName: string;
  dancers: string[] | string;
  timing: VideoTiming;
  contentType: VideoContentType;
  environment: VideoEnvironment;
  originType?: VideoOriginType | null;
  sourceUrl?: string | null;
  createdAt?: string | null;
  recordDate?: string | null;
  classWorkshop?: string | null;
  tags?: string[] | string | null;
  notes: string | null;
}) {
  const result = await mutateLibrary(async (library) => {
    const asset = library.videoAssets.find((entry) => entry.id === input.assetId && entry.kind === 'source');
    if (!asset) {
      throw new Error('Source asset not found');
    }
    const beforeAsset = sourceAssetAuditSnapshot(asset);
    const beforeAutoClipLabels = library.derivedClips
      .filter((clip) => clip.sourceAssetId === asset.id && !clip.manuallyNamed)
      .map((clip) => ({ id: clip.id, label: clip.label, updatedAt: clip.updatedAt }));

    asset.displayName = input.displayName.trim() || asset.displayName;
    asset.dancers = normalizeDancers(input.dancers);
    asset.timing = input.timing;
    asset.contentType = input.contentType;
    asset.environment = input.environment;
    asset.originType = input.originType === 'download' ? 'download' : 'self-recorded';
    asset.sourceUrl = asset.originType === 'download' ? normalizeOptionalText(input.sourceUrl) : null;
    asset.createdAt = normalizeDateString(input.createdAt) ?? asset.createdAt;
    asset.recordDate = normalizeDateString(input.recordDate);
    asset.classWorkshop = normalizeOptionalText(input.classWorkshop);
    asset.tags = normalizeTags(input.tags);
    asset.notes = input.notes?.trim() || null;

    const sourceClips = library.derivedClips.filter((clip) => clip.sourceAssetId === asset.id);
    sourceClips.forEach((clip, index) => {
      if (!clip.manuallyNamed) {
        clip.label = generatedClipLabel(asset, clip.moveDisplayId?.trim() || clip.moveId, index);
      }
      clip.updatedAt = nowIso();
    });
    sortLibrary(library);

    return {
      asset: structuredClone(asset),
      before: {
        asset: beforeAsset,
        autoClipLabels: beforeAutoClipLabels
      },
      after: {
        asset: sourceAssetAuditSnapshot(asset),
        autoClipLabels: library.derivedClips
          .filter((clip) => clip.sourceAssetId === asset.id && !clip.manuallyNamed)
          .map((clip) => ({ id: clip.id, label: clip.label, updatedAt: clip.updatedAt }))
      }
    };
  });
  recordVideoAuditAction({
    type: 'media.source.update',
    label: `Updated source video: ${result.asset.displayName}`,
    entityType: 'media:source',
    entityId: result.asset.id,
    before: result.before,
    after: result.after
  });
  return result.asset;
}

function runFfprobeCreationTime(filePath: string): Promise<string | null> {
  return new Promise((resolve) => {
    const ffprobe = spawn('ffprobe', [
      '-v',
      'quiet',
      '-print_format',
      'json',
      '-show_entries',
      'format_tags=creation_time:stream_tags=creation_time',
      filePath
    ]);

    let output = '';
    ffprobe.stdout.on('data', (chunk) => {
      output += chunk.toString();
    });

    ffprobe.once('error', () => resolve(null));
    ffprobe.once('close', (code) => {
      if (code !== 0) {
        resolve(null);
        return;
      }

      try {
        const parsed = JSON.parse(output);
        const candidates = [
          parsed?.format?.tags?.creation_time,
          ...(Array.isArray(parsed?.streams)
            ? parsed.streams.map((stream: { tags?: { creation_time?: string } }) => stream.tags?.creation_time)
            : [])
        ];
        const creationTime = candidates.find((value) => typeof value === 'string' && value.trim());
        resolve(creationTime ?? null);
      } catch {
        resolve(null);
      }
    });
  });
}

function dateOnlyFromDate(value: Date) {
  return Number.isNaN(value.getTime()) ? null : value.toISOString().slice(0, 10);
}

function dateOnlyFromText(value: string | null) {
  if (!value) return null;
  const date = new Date(value);
  return dateOnlyFromDate(date);
}

export async function detectSourceAssetFields(assetId: string, input?: { originType?: VideoOriginType | null }) {
  const library = await readLibraryFromDisk();
  const asset = library.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
  if (!asset) {
    throw new Error('Source asset not found');
  }

  const originType = input?.originType ?? asset.originType;
  if (originType !== 'self-recorded') {
    return {
      recordDate: null,
      message: 'Automatic field detection is only available for self-recorded source videos.'
    };
  }

  const absolutePath = resolveManagedVideoAbsolutePath(asset.filePath);
  const embeddedDate = dateOnlyFromText(await runFfprobeCreationTime(absolutePath));
  if (embeddedDate) {
    return {
      recordDate: embeddedDate,
      message: 'Detected record date from video metadata.'
    };
  }

  const stat = await fs.stat(absolutePath);
  const fileDate = dateOnlyFromDate(stat.birthtime) ?? dateOnlyFromDate(stat.mtime);
  return {
    recordDate: fileDate,
    message: fileDate
      ? 'Detected record date from the uploaded file timestamp.'
      : 'Could not detect a record date for this source video.'
  };
}

export async function deleteSourceAsset(assetId: string) {
  const job = upsertMediaJob({
    type: 'file.delete',
    targetType: 'videoAsset',
    targetId: assetId,
    idempotencyKey: `file.delete:source:${assetId}:${randomUUID()}`,
    payload: { assetId },
    maxAttempts: 1
  });

  let actionState: DeletedSourceMediaSnapshot | null = null;

  try {
    startMediaJob(job.id);
    const result = await mutateLibrary(async (library) => {
      const sourceAsset = library.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
      if (!sourceAsset) {
        throw new Error('Source asset not found');
      }

      const sourceClips = library.derivedClips.filter((clip) => clip.sourceAssetId === sourceAsset.id);
      const outputAssetIds = new Set(
        sourceClips
          .flatMap((clip) => [clip.outputAssetId, clip.publishedAssetId])
          .filter((id): id is string => Boolean(id))
      );
      const outputAssets = library.videoAssets.filter((asset) => outputAssetIds.has(asset.id));
      const assetIdsToDelete = new Set([sourceAsset.id, ...outputAssetIds]);
      const deletedAssetIds = Array.from(assetIdsToDelete);
      const deletedClipIds = sourceClips.map((clip) => clip.id);
      const moveLinks = library.moveVideoLinks.filter((link) => assetIdsToDelete.has(link.assetId));
      actionState = {
        mediaJobId: job.id,
        sourceAsset: structuredClone(sourceAsset),
        outputAssets: structuredClone(outputAssets),
        sourceClips: structuredClone(sourceClips),
        moveLinks: structuredClone(moveLinks),
        deletedAssetIds,
        deletedClipIds
      };

      const filePaths = [
        sourceAsset.filePath,
        ...outputAssets.map((asset) => asset.filePath),
        ...sourceClips.flatMap((clip) => [
          clip.actionOutputFilePath,
          clip.lowResOutputFilePath,
          clip.lowResPaddedOutputFilePath,
          clip.publishedActionOutputFilePath,
          clip.publishedLowResFilePath,
          clip.publishedLowResPaddedFilePath
        ])
      ].filter((filePath): filePath is string => Boolean(filePath));

      await trashManagedVideoFileSet({
        jobId: job.id,
        filePaths,
        actionType: 'move-to-trash',
        metadata: {
          sourceAssetId: sourceAsset.id
        }
      });

      library.derivedClips = library.derivedClips.filter((clip) => clip.sourceAssetId !== sourceAsset.id);
      library.moveVideoLinks = library.moveVideoLinks.filter((link) => !assetIdsToDelete.has(link.assetId));
      library.videoAssets = library.videoAssets.filter((asset) => !assetIdsToDelete.has(asset.id));
      sortLibrary(library);

      return {
        deletedAssetIds,
        deletedClipIds
      };
    });

    if (actionState) {
      runInTransaction((db) => {
        recordAction(db, {
          type: 'media.source.delete',
          label: `Deleted source video: ${actionState?.sourceAsset.displayName ?? assetId}`,
          entityType: 'media:source',
          entityId: assetId,
          before: actionState,
          after: {
            mediaJobId: job.id,
            deletedAssetIds: result.deletedAssetIds,
            deletedClipIds: result.deletedClipIds
          }
        });
      });
    }

    completeMediaJob(job.id);
    return result;
  } catch (error) {
    failMediaJob(job.id, error);
    if (actionState) {
      await restoreDeletedSourceMedia(actionState).catch(() => undefined);
    }
    throw error;
  }
}

type DeletedSourceMediaSnapshot = {
  mediaJobId: string;
  sourceAsset: VideoAsset;
  outputAssets: VideoAsset[];
  sourceClips: DerivedClip[];
  moveLinks: MoveVideoLink[];
  deletedAssetIds?: string[];
  deletedClipIds?: string[];
};

function isRecord(value: unknown): value is Record<string, unknown> {
  return Boolean(value && typeof value === 'object' && !Array.isArray(value));
}

function hasStringProperty(value: Record<string, unknown>, key: string) {
  return typeof value[key] === 'string' && value[key].trim().length > 0;
}

function validateDeletedSourceMediaSnapshot(state: unknown): DeletedSourceMediaSnapshot {
  if (!state || typeof state !== 'object') {
    throw new Error('Missing media delete snapshot.');
  }

  const snapshot = state as Record<string, unknown>;
  if (!hasStringProperty(snapshot, 'mediaJobId') || !isRecord(snapshot.sourceAsset)) {
    throw new Error('Invalid media delete snapshot.');
  }
  const sourceAsset = snapshot.sourceAsset;
  if (!hasStringProperty(sourceAsset, 'id') || !hasStringProperty(sourceAsset, 'filePath') || sourceAsset.kind !== 'source') {
    throw new Error('Invalid media delete snapshot.');
  }
  if (!Array.isArray(snapshot.outputAssets) || !Array.isArray(snapshot.sourceClips) || !Array.isArray(snapshot.moveLinks)) {
    throw new Error('Invalid media delete snapshot.');
  }
  const outputAssets = snapshot.outputAssets;
  const sourceClips = snapshot.sourceClips;
  const moveLinks = snapshot.moveLinks;
  const sourceAssetId = sourceAsset.id;
  if (
    outputAssets.some((asset) => !isRecord(asset) || !hasStringProperty(asset, 'id') || !hasStringProperty(asset, 'filePath')) ||
    sourceClips.some(
      (clip) =>
        !isRecord(clip) ||
        !hasStringProperty(clip, 'id') ||
        !hasStringProperty(clip, 'sourceAssetId') ||
        clip.sourceAssetId !== sourceAssetId
    ) ||
    moveLinks.some((link) => !isRecord(link) || !hasStringProperty(link, 'id') || !hasStringProperty(link, 'assetId'))
  ) {
    throw new Error('Invalid media delete snapshot.');
  }

  return {
    mediaJobId: String(snapshot.mediaJobId),
    sourceAsset: structuredClone(sourceAsset) as unknown as VideoAsset,
    outputAssets: structuredClone(outputAssets) as unknown as VideoAsset[],
    sourceClips: structuredClone(sourceClips) as unknown as DerivedClip[],
    moveLinks: structuredClone(moveLinks) as unknown as MoveVideoLink[],
    deletedAssetIds: Array.isArray(snapshot.deletedAssetIds) ? snapshot.deletedAssetIds.filter((id): id is string => typeof id === 'string') : undefined,
    deletedClipIds: Array.isArray(snapshot.deletedClipIds) ? snapshot.deletedClipIds.filter((id): id is string => typeof id === 'string') : undefined
  };
}

export async function restoreDeletedSourceMedia(state: unknown) {
  const snapshot = validateDeletedSourceMediaSnapshot(state);
  const { mediaJobId, sourceAsset, outputAssets, sourceClips, moveLinks } = snapshot;

  await restoreTrashedFilesForJob(mediaJobId);

  return mutateLibrary((library) => {
    const assetIds = new Set([sourceAsset.id, ...outputAssets.map((asset) => asset.id)]);
    const clipIds = new Set(sourceClips.map((clip) => clip.id));
    const linkIds = new Set(moveLinks.map((link) => link.id));

    library.videoAssets = library.videoAssets.filter((asset) => !assetIds.has(asset.id));
    library.derivedClips = library.derivedClips.filter((clip) => !clipIds.has(clip.id));
    library.moveVideoLinks = library.moveVideoLinks.filter((link) => !linkIds.has(link.id) && !assetIds.has(link.assetId));
    library.videoAssets.push(structuredClone(sourceAsset), ...structuredClone(outputAssets));
    library.derivedClips.push(...structuredClone(sourceClips));
    library.moveVideoLinks.push(...structuredClone(moveLinks));
    sortLibrary(library);

    return {
      restoredAssetIds: [...assetIds],
      restoredClipIds: [...clipIds]
    };
  });
}

async function runSourceHashJob(job: MediaJob) {
  const assetId = job.targetId;
  try {
    startMediaJob(job.id);
    const library = await readLibraryFromDisk();
    const sourceAsset = library.videoAssets.find((asset) => asset.id === assetId && asset.kind === 'source');
    if (!sourceAsset) {
      throw new Error('Source asset not found.');
    }

    const absolutePath = resolveManagedVideoAbsolutePath(sourceAsset.filePath);
    const fingerprint = await hashFile(absolutePath);
    await mutateLibrary((mutableLibrary) => {
      const asset = mutableLibrary.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
      if (!asset) {
        throw new Error('Source asset not found.');
      }
      asset.contentHash = fingerprint.contentHash;
      asset.contentHashAlgorithm = fingerprint.contentHashAlgorithm;
      asset.contentSizeBytes = fingerprint.contentSizeBytes;
      asset.hashStatus = 'ready';
      sortLibrary(mutableLibrary);
    });

    completeMediaJob(job.id);
    recordMediaFileAction({
      jobId: job.id,
      actionType: 'hash-source',
      status: 'succeeded',
      filePath: sourceAsset.filePath,
      metadata: {
        contentHash: fingerprint.contentHash,
        contentSizeBytes: fingerprint.contentSizeBytes
      }
    });
  } catch (error) {
    failMediaJob(job.id, error);
    await mutateLibrary((library) => {
      const asset = library.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
      if (asset) {
        asset.hashStatus = 'failed';
      }
    });
  }
}

async function drainSourceHashQueue() {
  if (sourceHashWorkerRunning) {
    return;
  }

  sourceHashWorkerRunning = true;
  try {
    while (true) {
      const job = listQueuedMediaJobs('source.hash', 1)[0] ?? null;
      if (!job) {
        return;
      }
      await runSourceHashJob(job);
    }
  } finally {
    sourceHashWorkerRunning = false;
  }
}

export async function queueSourceHash(assetId: string) {
  const library = await readLibraryFromDisk();
  const asset = library.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
  if (!asset) {
    throw new Error('Source asset not found.');
  }

  await mutateLibrary((mutableLibrary) => {
    const mutableAsset = mutableLibrary.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
    if (mutableAsset) {
      mutableAsset.hashStatus = 'pending';
    }
  });

  const job = upsertMediaJob({
    type: 'source.hash',
    targetType: 'videoAsset',
    targetId: assetId,
    idempotencyKey: `source.hash:${assetId}:backfill`,
    payload: { assetId, filePath: asset.filePath },
    retryFailed: true,
    retryCompleted: true
  });

  void drainSourceHashQueue();
  return job;
}

export async function queueSourceHashBackfill() {
  const library = await readLibraryFromDisk();
  const assets = library.videoAssets.filter(
    (asset) => asset.kind === 'source' && (!asset.contentHash || asset.hashStatus !== 'ready')
  );
  const jobs: MediaJob[] = [];
  for (const asset of assets) {
    jobs.push(await queueSourceHash(asset.id));
  }
  void drainSourceHashQueue();
  return {
    queued: jobs.length,
    jobIds: jobs.map((job) => job.id)
  };
}

export function listMediaManagerJobs(limit = 100) {
  return listMediaJobsWithFileActions(limit);
}

export async function retryMediaManagerJob(jobId: string) {
  const job = getMediaJobById(jobId);
  if (!job) {
    throw new Error('Media job not found.');
  }

  if (job.type === 'clip.render') {
    return queueClipRender(job.targetId, { audit: false });
  }
  if (job.type === 'poster.generate') {
    await queuePosterGeneration(job.targetId);
    return getMediaJobById(job.id) ?? job;
  }
  if (job.type === 'source.hash') {
    return queueSourceHash(job.targetId);
  }

  throw new Error('This media job type cannot be retried from the UI.');
}

export async function saveSourceClips(input: {
  sourceAssetId: string;
  clips: Array<{
    id?: string;
    moveId: string;
    moveDisplayId?: string | null;
    isKeyVideo?: boolean;
    label?: string | null;
    descriptorLabel?: string | null;
    startPositionId?: string | null;
    endPositionId?: string | null;
    timingGroupId?: string | null;
    manuallyNamed?: boolean;
    startMs: number;
    endMs: number;
    actionStartMs?: number | null;
    actionEndMs?: number | null;
    cropRect?: ClipCropRect | null;
    countMarkers?: ClipCountMarker[];
    countOverlayPlacement?: CountOverlayPlacement;
    countTimingPreset?: CountTimingPreset;
  }>;
}) {
  const result = await mutateLibrary(async (library) => {
    const sourceAsset = library.videoAssets.find((asset) => asset.id === input.sourceAssetId && asset.kind === 'source');
    if (!sourceAsset) {
      throw new Error('Source asset not found');
    }

    const existingById = new Map(
      library.derivedClips.filter((clip) => clip.sourceAssetId === input.sourceAssetId).map((clip) => [clip.id, clip])
    );
    const beforeClips = Array.from(existingById.values()).map(derivedClipAuditSnapshot);

    const nextClipsWithoutKeyDefaults = input.clips.map((clipInput, index) => {
      const existing = clipInput.id ? existingById.get(clipInput.id) ?? null : null;
      const moveId = clipInput.moveId.trim().toUpperCase();
      const moveDisplayId = String(clipInput.moveDisplayId ?? existing?.moveDisplayId ?? moveId).trim().toUpperCase() || moveId;
      const manuallyNamed = Boolean(clipInput.manuallyNamed || (existing?.manuallyNamed && clipInput.label?.trim()));
      const isKeyVideo = clipInput.isKeyVideo === undefined ? existing?.isKeyVideo : Boolean(clipInput.isKeyVideo);
      const label = manuallyNamed ? clipInput.label?.trim() || existing?.label || null : generatedClipLabel(sourceAsset, moveDisplayId, index);
      const descriptorLabel = normalizeOptionalText(clipInput.descriptorLabel);
      const startPositionId = normalizeOptionalText(clipInput.startPositionId);
      const endPositionId = normalizeOptionalText(clipInput.endPositionId);
      const timingGroupId = normalizeOptionalText(clipInput.timingGroupId);
      const startMs = Math.max(0, Math.floor(clipInput.startMs));
      const endMs = Math.max(0, Math.floor(clipInput.endMs));
      const actionStartMs =
        clipInput.actionStartMs === null || clipInput.actionStartMs === undefined
          ? existing?.actionStartMs ?? null
          : Math.max(0, Math.floor(clipInput.actionStartMs));
      const actionEndMs =
        clipInput.actionEndMs === null || clipInput.actionEndMs === undefined
          ? existing?.actionEndMs ?? null
          : Math.max(0, Math.floor(clipInput.actionEndMs));
      const cropRect = clipInput.cropRect === undefined ? existing?.cropRect ?? null : normalizeCropRect(clipInput.cropRect);
      const countMarkers = clipInput.countMarkers ? normalizeCountMarkers(clipInput.countMarkers) : existing?.countMarkers ?? [];
      const countOverlayPlacement = isCountOverlayPlacement(clipInput.countOverlayPlacement)
        ? clipInput.countOverlayPlacement
        : existing?.countOverlayPlacement ?? 'top-left';
      const countTimingPreset = isCountTimingPreset(clipInput.countTimingPreset)
        ? clipInput.countTimingPreset
        : existing?.countTimingPreset ?? 'on2-default';
      const renderChanged =
        !existing ||
        existing.moveId !== moveId ||
        existing.startMs !== startMs ||
        existing.endMs !== endMs ||
        existing.actionStartMs !== actionStartMs ||
        existing.actionEndMs !== actionEndMs ||
        !cropRectsEqual(existing.cropRect, cropRect);
      return {
        id: existing?.id ?? randomUUID(),
        sourceAssetId: input.sourceAssetId,
        moveId,
        moveDisplayId,
        isKeyVideo,
        label,
        descriptorLabel,
        startPositionId,
        endPositionId,
        timingGroupId,
        manuallyNamed,
        startMs,
        endMs,
        actionStartMs,
        actionEndMs,
        cropRect,
        countMarkers,
        countOverlayPlacement,
        countTimingPreset,
        outputAssetId: existing?.outputAssetId ?? null,
        actionOutputFilePath: existing?.actionOutputFilePath ?? null,
        lowResOutputFilePath: existing?.lowResOutputFilePath ?? null,
        lowResPaddedOutputFilePath: existing?.lowResPaddedOutputFilePath ?? null,
        publishedAssetId: existing?.publishedAssetId ?? null,
        publishedActionOutputFilePath: existing?.publishedActionOutputFilePath ?? null,
        publishedLowResFilePath: existing?.publishedLowResFilePath ?? null,
        publishedLowResPaddedFilePath: existing?.publishedLowResPaddedFilePath ?? null,
        publishedAt: existing?.publishedAt ?? null,
        status: renderChanged ? 'pending' : existing?.status ?? 'pending',
        error: renderChanged ? null : existing?.error ?? null,
        createdAt: existing?.createdAt ?? nowIso(),
        updatedAt: existing?.updatedAt ?? nowIso()
      };
    });
    const nextClipsWithKeyDefaults = applyDefaultKeyVideoFlags(
      library.derivedClips,
      nextClipsWithoutKeyDefaults,
      input.sourceAssetId
    ) as DerivedClip[];
    const nextClips: DerivedClip[] = nextClipsWithKeyDefaults.map((clip) => {
      const existing = existingById.get(clip.id) ?? null;
      const metadataChanged =
        !existing ||
        existing.isKeyVideo !== clip.isKeyVideo ||
        existing.descriptorLabel !== clip.descriptorLabel ||
        existing.startPositionId !== clip.startPositionId ||
        existing.endPositionId !== clip.endPositionId ||
        existing.timingGroupId !== clip.timingGroupId ||
        !countMarkersEqual(existing.countMarkers, clip.countMarkers) ||
        existing.countOverlayPlacement !== clip.countOverlayPlacement ||
        existing.countTimingPreset !== clip.countTimingPreset;
      const renderChanged =
        !existing ||
        existing.moveId !== clip.moveId ||
        existing.startMs !== clip.startMs ||
        existing.endMs !== clip.endMs ||
        existing.actionStartMs !== clip.actionStartMs ||
        existing.actionEndMs !== clip.actionEndMs ||
        !cropRectsEqual(existing.cropRect, clip.cropRect);
      const changed = renderChanged || metadataChanged || existing?.label !== clip.label || existing?.manuallyNamed !== clip.manuallyNamed;
      return {
        ...clip,
        updatedAt: changed ? nowIso() : clip.updatedAt
      };
    });

    const nextClipIds = new Set(nextClips.map((clip) => clip.id));
    const removedClips = Array.from(existingById.values()).filter((clip) => !nextClipIds.has(clip.id));
    const removedAssetIds = new Set(
      removedClips
        .flatMap((clip) => [clip.outputAssetId, clip.publishedAssetId])
        .filter((id): id is string => Boolean(id))
    );
    const removedAssets = library.videoAssets.filter((asset) => removedAssetIds.has(asset.id));
    const removedFilePaths = removedClips
      .flatMap((clip) => [
        clip.actionOutputFilePath,
        clip.lowResOutputFilePath,
        clip.lowResPaddedOutputFilePath,
        clip.publishedActionOutputFilePath,
        clip.publishedLowResFilePath,
        clip.publishedLowResPaddedFilePath
      ])
      .filter((filePath): filePath is string => Boolean(filePath));

    library.derivedClips = library.derivedClips.filter((clip) => clip.sourceAssetId !== input.sourceAssetId).concat(nextClips);
    library.moveVideoLinks = library.moveVideoLinks.filter((link) => !removedAssetIds.has(link.assetId));
    library.videoAssets = library.videoAssets.filter((asset) => !removedAssetIds.has(asset.id));
    const publishedAt = nowIso();
    nextClips.forEach((clip) => {
      if (clip.status === 'ready' && clip.outputAssetId && clipPublicationStatus(clip) !== 'modern-published') {
        publishClipToMove(library, clip, publishedAt);
      }
    });
    sortLibrary(library);
    const cleanupFilePaths = [...removedAssets.map((asset) => asset.filePath), ...removedFilePaths];
    const cleanupJobIds: string[] = [];
    if (cleanupFilePaths.length) {
      const cleanupJob = createMediaCleanupJob({
        targetType: 'sourceAsset',
        targetId: input.sourceAssetId,
        payload: {
          cleanupReason: 'removed-source-clips',
          removedClipIds: removedClips.map((clip) => clip.id),
          removedAssetIds: [...removedAssetIds],
          filePaths: cleanupFilePaths
        }
      });
      cleanupJobIds.push(cleanupJob.id);
      startMediaJob(cleanupJob.id);
      try {
        await trashManagedVideoFileSet({
          jobId: cleanupJob.id,
          filePaths: cleanupFilePaths,
          actionType: 'cleanup-generated',
          metadata: {
            sourceAssetId: input.sourceAssetId,
            cleanupReason: 'removed-source-clips'
          }
        });
        completeMediaJob(cleanupJob.id);
      } catch (error) {
        failMediaJob(cleanupJob.id, error);
        throw error;
      }
    }
    return {
      clips: nextClips,
      before: {
        sourceAssetId: input.sourceAssetId,
        clips: beforeClips
      },
      after: {
        sourceAssetId: input.sourceAssetId,
        clips: nextClips.map(derivedClipAuditSnapshot),
        removedClipIds: removedClips.map((clip) => clip.id),
        removedAssetIds: [...removedAssetIds],
        cleanupJobIds
      }
    };
  });
  recordVideoAuditAction({
    type: 'media.clips.save',
    label: `Saved source clips: ${input.sourceAssetId}`,
    entityType: 'media:source',
    entityId: input.sourceAssetId,
    before: result.before,
    after: result.after
  });
  return result.clips;
}

export async function setClipKeyVideo(input: { clipId: string; isKeyVideo: boolean }) {
  const result = await mutateLibrary(async (library) => {
    const clip = library.derivedClips.find((entry) => entry.id === input.clipId);
    if (!clip) {
      throw new Error('Clip not found');
    }

    const before = {
      clipId: clip.id,
      sourceAssetId: clip.sourceAssetId,
      moveId: clip.moveId,
      isKeyVideo: clip.isKeyVideo,
      updatedAt: clip.updatedAt
    };
    clip.isKeyVideo = Boolean(input.isKeyVideo);
    clip.updatedAt = nowIso();
    sortLibrary(library);
    return {
      clip: { ...clip },
      before,
      after: {
        clipId: clip.id,
        sourceAssetId: clip.sourceAssetId,
        moveId: clip.moveId,
        isKeyVideo: clip.isKeyVideo,
        updatedAt: clip.updatedAt
      }
    };
  });
  recordVideoAuditAction({
    type: 'media.clip.key.update',
    label: `Updated key video: ${input.clipId}`,
    entityType: 'media:clip',
    entityId: input.clipId,
    before: result.before,
    after: result.after
  });
  return result.clip;
}

function clipOutputRelativePath(moveId: string, sourceDisplayName: string, clipId: string, suffix = '') {
  const safeDisplay = sanitizeFilenamePart(sourceDisplayName) || 'clip';
  const safeSuffix = sanitizeFilenamePart(suffix);
  return `${moveId} ${safeDisplay} ${clipId.slice(0, 8)}${safeSuffix ? ` ${safeSuffix}` : ''}${CLIP_OUTPUT_EXTENSION}`;
}

function videoFilterArgs(cropRect: ClipCropRect | null, lowRes: boolean) {
  const filters: string[] = [];

  if (cropRect) {
    filters.push(
      [
        `crop=iw*${cropRect.width.toFixed(6)}`,
        `ih*${cropRect.height.toFixed(6)}`,
        `iw*${cropRect.x.toFixed(6)}`,
        `ih*${cropRect.y.toFixed(6)}`
      ].join(':')
    );
  }

  if (lowRes) {
    filters.push('scale=-2:360');
  }

  return filters.join(',');
}

async function renderVideoSegment(
  inputAbsolutePath: string,
  outputAbsolutePath: string,
  startMs: number,
  durationMs: number,
  lowRes = false,
  cropRect: ClipCropRect | null = null
) {
  await fs.mkdir(path.dirname(outputAbsolutePath), { recursive: true });

  const args = [
    '-hide_banner',
    '-loglevel',
    'error',
    '-y',
    '-ss',
    String((startMs / 1000).toFixed(3)),
    '-t',
    String((durationMs / 1000).toFixed(3)),
    '-i',
    inputAbsolutePath,
    '-c:v',
    'libx264',
    '-preset',
    lowRes ? 'veryfast' : 'medium',
    '-crf',
    lowRes ? LOW_QUALITY_CRF : FULL_QUALITY_CRF
  ];

  const filters = videoFilterArgs(cropRect, lowRes);
  if (filters) {
    args.push('-vf', filters);
  }

  args.push('-c:a', 'aac', '-movflags', '+faststart', outputAbsolutePath);

  await new Promise<void>((resolve, reject) => {
    const ffmpeg = spawn('ffmpeg', args, { stdio: 'ignore' });

    ffmpeg.once('error', reject);
    ffmpeg.once('close', (code) => {
      if (code === 0) {
        resolve();
        return;
      }
      reject(new Error(`ffmpeg exited with code ${code}`));
    });
  });
}

export async function cleanupObsoleteRenderedClipFiles(input: {
  jobId: string;
  clipId: string;
  replacedRenderedFilePaths: string[];
  currentRenderedFilePaths: string[];
}) {
  const deletedFilePaths = obsoleteGeneratedClipFilePaths(
    input.replacedRenderedFilePaths,
    input.currentRenderedFilePaths
  );
  if (deletedFilePaths.length) {
    await trashManagedVideoFileSet({
      jobId: input.jobId,
      filePaths: deletedFilePaths,
      actionType: 'cleanup-generated',
      metadata: {
        clipId: input.clipId,
        cleanupReason: 'obsolete-render'
      }
    });
  }
  return deletedFilePaths;
}

async function renderClip(clipId: string, jobId: string) {
  const library = await readLibraryFromDisk();
  const clip = library.derivedClips.find((entry) => entry.id === clipId);
  if (!clip) {
    throw new Error('Clip not found');
  }

  const sourceAsset = library.videoAssets.find((entry) => entry.id === clip.sourceAssetId);
  if (!sourceAsset) {
    throw new Error('Source asset not found');
  }

  const durationMs = Math.max(1, clip.endMs - clip.startMs);
  const actionStartMs = clip.actionStartMs ?? clip.startMs;
  const actionEndMs = clip.actionEndMs ?? clip.endMs;
  const actionDurationMs = Math.max(1, actionEndMs - actionStartMs);
  const needsDraftAsset = Boolean(clip.publishedAssetId && clip.outputAssetId === clip.publishedAssetId);
  const draftSuffix = needsDraftAsset ? `draft ${Date.now().toString(36)}` : '';
  const clipDisplayId = clip.moveDisplayId?.trim() || clip.moveId;
  const outputFilename = clipOutputRelativePath(
    clipDisplayId,
    sourceAsset.displayName,
    clip.id,
    draftSuffix
  );
  const outputRelativePath = path.posix.join(MOVE_VIDEO_PREFIX, outputFilename);
  const actionOutputRelativePath = path.posix.join(
    MOVE_VIDEO_PREFIX,
    clipOutputRelativePath(clipDisplayId, sourceAsset.displayName, clip.id, `${draftSuffix ? `${draftSuffix} ` : ''}action`)
  );
  const lowResOutputRelativePath = path.posix.join(
    MOVE_VIDEO_PREFIX,
    clipOutputRelativePath(clipDisplayId, sourceAsset.displayName, clip.id, `${draftSuffix ? `${draftSuffix} ` : ''}low`)
  );
  const lowResPaddedOutputRelativePath = path.posix.join(
    MOVE_VIDEO_PREFIX,
    clipOutputRelativePath(
      clipDisplayId,
      sourceAsset.displayName,
      clip.id,
      `${draftSuffix ? `${draftSuffix} ` : ''}padded low`
    )
  );
  const outputAbsolutePath = resolveManagedVideoAbsolutePath(outputRelativePath);
  const actionOutputAbsolutePath = resolveManagedVideoAbsolutePath(actionOutputRelativePath);
  const lowResOutputAbsolutePath = resolveManagedVideoAbsolutePath(lowResOutputRelativePath);
  const lowResPaddedOutputAbsolutePath = resolveManagedVideoAbsolutePath(lowResPaddedOutputRelativePath);
  const inputAbsolutePath = resolveManagedVideoAbsolutePath(sourceAsset.filePath);

  await mutateLibrary((mutableLibrary) => {
    const mutableClip = mutableLibrary.derivedClips.find((entry) => entry.id === clipId);
    if (mutableClip) {
      mutableClip.status = 'rendering';
      mutableClip.error = null;
      mutableClip.updatedAt = nowIso();
    }
  });

  await renderVideoSegment(inputAbsolutePath, outputAbsolutePath, clip.startMs, durationMs, false, clip.cropRect);
  await renderVideoSegment(inputAbsolutePath, actionOutputAbsolutePath, actionStartMs, actionDurationMs, false, clip.cropRect);
  await renderVideoSegment(inputAbsolutePath, lowResPaddedOutputAbsolutePath, clip.startMs, durationMs, true, clip.cropRect);
  await renderVideoSegment(inputAbsolutePath, lowResOutputAbsolutePath, actionStartMs, actionDurationMs, true, clip.cropRect);

  const replacedRenderedFilePaths = new Set<string>();
  const currentRenderedFilePaths = [
    outputRelativePath,
    actionOutputRelativePath,
    lowResOutputRelativePath,
    lowResPaddedOutputRelativePath
  ];
  await mutateLibrary((mutableLibrary) => {
    const mutableClip = mutableLibrary.derivedClips.find((entry) => entry.id === clipId);
    const source = mutableLibrary.videoAssets.find((entry) => entry.id === clip?.sourceAssetId);
    if (!mutableClip || !source) {
      return;
    }

    const previousAssetIds = new Set(
      [mutableClip.outputAssetId, mutableClip.publishedAssetId].filter((id): id is string => Boolean(id))
    );
    const previousVariantPaths = [
      mutableClip.actionOutputFilePath,
      mutableClip.lowResOutputFilePath,
      mutableClip.lowResPaddedOutputFilePath,
      mutableClip.publishedActionOutputFilePath,
      mutableClip.publishedLowResFilePath,
      mutableClip.publishedLowResPaddedFilePath
    ].filter((filePath): filePath is string => Boolean(filePath));
    let outputAsset =
      mutableClip.outputAssetId && mutableClip.outputAssetId !== mutableClip.publishedAssetId
      ? mutableLibrary.videoAssets.find((entry) => entry.id === mutableClip.outputAssetId) ?? null
      : null;

    if (!outputAsset) {
      outputAsset = {
        id: randomUUID(),
        kind: 'move',
        filePath: outputRelativePath,
        displayName: mutableClip.label?.trim() || source.displayName,
        originalFilename: path.basename(outputRelativePath),
        dancers: [...source.dancers],
        timing: source.timing,
        contentType: source.contentType,
        environment: source.environment,
        originType: source.originType,
        sourceUrl: source.sourceUrl,
        recordDate: source.recordDate,
        classWorkshop: source.classWorkshop,
        tags: [...source.tags],
        notes: source.notes,
        contentHash: null,
        contentHashAlgorithm: null,
        contentSizeBytes: null,
        hashStatus: 'pending',
        createdAt: nowIso()
      };
      mutableLibrary.videoAssets.push(outputAsset);
      mutableClip.outputAssetId = outputAsset.id;
    } else {
      outputAsset.filePath = outputRelativePath;
      outputAsset.displayName = mutableClip.label?.trim() || source.displayName;
      outputAsset.originalFilename = path.basename(outputRelativePath);
      outputAsset.dancers = [...source.dancers];
      outputAsset.timing = source.timing;
      outputAsset.contentType = source.contentType;
      outputAsset.environment = source.environment;
      outputAsset.originType = source.originType;
      outputAsset.sourceUrl = source.sourceUrl;
      outputAsset.recordDate = source.recordDate;
      outputAsset.classWorkshop = source.classWorkshop;
      outputAsset.tags = [...source.tags];
      outputAsset.notes = source.notes;
    }

    const publishedAt = nowIso();
    mutableClip.status = 'ready';
    mutableClip.error = null;
    mutableClip.actionOutputFilePath = actionOutputRelativePath;
    mutableClip.lowResOutputFilePath = lowResOutputRelativePath;
    mutableClip.lowResPaddedOutputFilePath = lowResPaddedOutputRelativePath;
    mutableClip.updatedAt = publishedAt;
    publishClipToMove(mutableLibrary, mutableClip, publishedAt);
    previousAssetIds.delete(outputAsset.id);
    const replacedAssetIds = previousAssetIds;
    mutableLibrary.videoAssets
      .filter((asset) => replacedAssetIds.has(asset.id))
      .forEach((asset) => replacedRenderedFilePaths.add(asset.filePath));
    previousVariantPaths.forEach((filePath) => replacedRenderedFilePaths.add(filePath));
    mutableLibrary.moveVideoLinks = mutableLibrary.moveVideoLinks.filter((link) => !replacedAssetIds.has(link.assetId));
    mutableLibrary.videoAssets = mutableLibrary.videoAssets.filter((asset) => !replacedAssetIds.has(asset.id));
    sortLibrary(mutableLibrary);
  });

  const deletedFilePaths = await cleanupObsoleteRenderedClipFiles({
    jobId,
    clipId,
    replacedRenderedFilePaths: [...replacedRenderedFilePaths],
    currentRenderedFilePaths
  });
  void queuePosterGeneration(outputRelativePath);
  return {
    outputRelativePath,
    actionOutputRelativePath,
    lowResOutputRelativePath,
    lowResPaddedOutputRelativePath,
    deletedFilePaths
  };
}

async function runRenderJob(job: MediaJob) {
  const clipId = job.targetId;
  try {
    startMediaJob(job.id);
    const result = await renderClip(clipId, job.id);
    completeMediaJob(job.id);
    [
      ['render-full-quality-padded', result.outputRelativePath],
      ['render-full-quality-action', result.actionOutputRelativePath],
      ['render-preview-action', result.lowResOutputRelativePath],
      ['render-preview-padded', result.lowResPaddedOutputRelativePath]
    ].forEach(([actionType, filePath]) => {
      recordMediaFileAction({
        jobId: job.id,
        actionType,
        status: 'succeeded',
        filePath
      });
    });
  } catch (error) {
    failMediaJob(job.id, error);
    await mutateLibrary((library) => {
      const clip = library.derivedClips.find((entry) => entry.id === clipId);
      if (clip) {
        clip.status = 'failed';
        clip.error = error instanceof Error ? error.message : 'Render failed';
        clip.updatedAt = nowIso();
      }
    });
  }
}

async function drainRenderQueue() {
  if (renderWorkerRunning) {
    return;
  }

  renderWorkerRunning = true;
  try {
    while (true) {
      const job = listQueuedMediaJobs('clip.render', 1)[0] ?? null;
      if (!job) {
        return;
      }
      await runRenderJob(job);
    }
  } finally {
    renderWorkerRunning = false;
  }
}

export function queueClipRender(clipId: string, options: { audit?: boolean } = {}) {
  const job = upsertMediaJob({
    type: 'clip.render',
    targetType: 'derivedClip',
    targetId: clipId,
    idempotencyKey: `clip.render:${clipId}`,
    payload: { clipId },
    retryFailed: true,
    retryCompleted: true
  });

  if (options.audit !== false) {
    recordVideoAuditAction({
      type: 'media.clip.render.queue',
      label: `Queued clip render: ${clipId}`,
      entityType: 'media:clip',
      entityId: clipId,
      before: {
        clipId
      },
      after: {
        clipId,
        jobId: job.id,
        jobStatus: job.status
      }
    });
  }

  void drainRenderQueue();
  return job;
}

export function isClipRenderPending(clipId: string) {
  return isMediaJobTargetPending('clip.render', 'derivedClip', clipId);
}

export async function publishClipsToMoves(clipIds: string[]) {
  const requestedIds = new Set(clipIds.map((clipId) => clipId.trim()).filter(Boolean));
  if (!requestedIds.size) {
    return [];
  }

  const result = await mutateLibrary((library) => {
    const publishedAt = nowIso();
    const publishedClips: DerivedClip[] = [];
    const beforeClips = library.derivedClips
      .filter((clip) => requestedIds.has(clip.id))
      .map(derivedClipAuditSnapshot);
    const beforeAssetIds = new Set(
      library.derivedClips
        .filter((clip) => requestedIds.has(clip.id))
        .flatMap((clip) => [clip.outputAssetId, clip.publishedAssetId])
        .filter((id): id is string => Boolean(id))
    );
    const beforeLinks = library.moveVideoLinks
      .filter((link) => beforeAssetIds.has(link.assetId))
      .map(moveVideoLinkAuditSnapshot);

    library.derivedClips.forEach((clip) => {
      if (!requestedIds.has(clip.id)) {
        return;
      }

      publishedClips.push({ ...publishClipToMove(library, clip, publishedAt) });
    });

    sortLibrary(library);
    const afterAssetIds = new Set(
      publishedClips
        .flatMap((clip) => [clip.outputAssetId, clip.publishedAssetId])
        .filter((id): id is string => Boolean(id))
    );
    return {
      clips: publishedClips,
      before: {
        requestedClipIds: [...requestedIds],
        clips: beforeClips,
        moveVideoLinks: beforeLinks
      },
      after: {
        requestedClipIds: [...requestedIds],
        publishedClipIds: publishedClips.map((clip) => clip.id),
        clips: publishedClips.map(derivedClipAuditSnapshot),
        moveVideoLinks: library.moveVideoLinks
          .filter((link) => afterAssetIds.has(link.assetId))
          .map(moveVideoLinkAuditSnapshot)
      }
    };
  });
  recordVideoAuditAction({
    type: 'media.clips.publish',
    label: `Published clips: ${result.clips.length}`,
    entityType: 'media:clips',
    entityId: [...requestedIds].sort().join(','),
    before: result.before,
    after: result.after
  });
  return result.clips;
}

export async function getSourceAssets(moves?: MoveRecord[]) {
  const library = await getVideoLibrary(moves);
  return library.videoAssets.filter((asset) => asset.kind === 'source');
}

export async function getUploadPageData(moves: MoveRecord[]) {
  const library = await getVideoLibrary(moves);
  const assets = await Promise.all(
    library.videoAssets
      .filter((asset) => asset.kind === 'source')
      .map(async (asset) => {
        const clips = library.derivedClips.filter((clip) => clip.sourceAssetId === asset.id);
        const linkedMoveIds = Array.from(new Set(clips.map((clip) => clip.moveId))).sort((left, right) =>
          left.localeCompare(right)
        );
        return {
          ...asset,
          posterFile: await findPosterForVideoFile(asset.filePath),
          linkedMoveIds,
          clips
        };
      })
  );

  return {
    assets,
    library
  };
}

type MediaLibraryPageFilter = {
  limit?: number;
  cursor?: string | null;
  publication?: 'all' | 'published' | 'unpublished' | 'draft';
  environment?: 'all' | VideoEnvironment;
  dancers?: string[];
};

function clipPublicationStatus(clip: DerivedClip) {
  return publicationStatusFor({
    isModern: true,
    publishedAt: clip.publishedAt,
    updatedAt: clip.updatedAt
  });
}

function sourceAssetPublicationStatus(asset: VideoAsset, clips: DerivedClip[]) {
  const sourceClips = clips.filter((clip) => clip.sourceAssetId === asset.id);
  if (!sourceClips.length) return 'unpublished';
  if (sourceClips.some((clip) => clipPublicationStatus(clip) === 'changed-unpublished')) return 'draft';
  if (sourceClips.some((clip) => clipPublicationStatus(clip) === 'modern-published')) return 'published';
  return 'unpublished';
}

export async function getMediaLibraryPage(moves: MoveRecord[], input?: MediaLibraryPageFilter) {
  const library = await getVideoLibrary(moves);
  const limit = Math.max(1, Math.min(100, Math.floor(input?.limit ?? 50)));
  const offset = Math.max(0, Number.parseInt(input?.cursor ?? '0', 10) || 0);
  const publication = input?.publication ?? 'all';
  const environment = input?.environment ?? 'all';
  const selectedDancers = new Set((input?.dancers ?? []).map((dancer) => dancer.trim()).filter(Boolean));
  const sourceAssets = library.videoAssets
    .filter((asset) => asset.kind === 'source')
    .filter((asset) => {
      if (publication !== 'all' && sourceAssetPublicationStatus(asset, library.derivedClips) !== publication) return false;
      if (environment !== 'all' && asset.environment !== environment) return false;
      if (selectedDancers.size && !Array.from(selectedDancers).every((dancer) => asset.dancers.includes(dancer))) return false;
      return true;
    })
    .sort((left, right) => {
      const byDate = right.createdAt.localeCompare(left.createdAt);
      return byDate || right.displayName.localeCompare(left.displayName);
    });
  const dancerOptions = Array.from(
    new Set(library.videoAssets.filter((asset) => asset.kind === 'source').flatMap((asset) => asset.dancers))
  ).sort((left, right) => left.localeCompare(right));
  const pageAssets = sourceAssets.slice(offset, offset + limit);
  const assets = await Promise.all(
    pageAssets.map(async (asset) => {
      const clips = library.derivedClips.filter((clip) => clip.sourceAssetId === asset.id);
      const linkedMoveIds = Array.from(new Set(clips.map((clip) => clip.moveId))).sort((left, right) =>
        left.localeCompare(right)
      );

      return {
        ...asset,
        posterFile: await findPosterForVideoFile(asset.filePath),
        linkedMoveIds,
        clips
      };
    })
  );

  const groups = assets.reduce(
    (result, asset) => {
      const month = uploadMonthKey(asset.createdAt);
      let group = result.find((entry) => entry.month === month);
      if (!group) {
        group = { month, assets: [] as typeof assets };
        result.push(group);
      }
      group.assets.push(asset);
      return result;
    },
    [] as Array<{ month: string; assets: typeof assets }>
  );

  return {
    assets,
    groups,
    total: sourceAssets.length,
    nextCursor: offset + limit < sourceAssets.length ? String(offset + limit) : null,
    dancerOptions,
    suggestions: sourceSuggestions(library.videoAssets)
  };
}

export async function getRenderStatuses(clipIds: string[]) {
  const library = await readLibraryFromDisk();
  return clipIds.map((clipId) => {
    const clip = library.derivedClips.find((entry) => entry.id === clipId) ?? null;
    return {
      clipId,
      status: clip?.status ?? 'failed',
      error: clip?.error ?? null,
      outputAssetId: clip?.outputAssetId ?? null,
      actionOutputFilePath: clip?.actionOutputFilePath ?? null,
      lowResOutputFilePath: clip?.lowResOutputFilePath ?? null,
      lowResPaddedOutputFilePath: clip?.lowResPaddedOutputFilePath ?? null,
      publishedActionOutputFilePath: clip?.publishedActionOutputFilePath ?? null,
      publishedLowResFilePath: clip?.publishedLowResFilePath ?? null,
      publishedLowResPaddedFilePath: clip?.publishedLowResPaddedFilePath ?? null,
      pending: isClipRenderPending(clipId)
    };
  });
}

export async function getVideoLibrarySummary(moves: MoveRecord[]) {
  const { assets } = await getUploadPageData(moves);
  return assets;
}
