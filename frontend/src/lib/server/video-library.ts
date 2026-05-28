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
  managedPosterPathForVideo,
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
  normalizeDateString,
  normalizeOptionalText,
  normalizeTags,
  sourceSuggestions,
  uploadMonthKey
} from '$lib/video-library-utils';
import { publicationStatusFor } from '$lib/content-status';

const LIBRARY_FILENAME = 'video-library.json';
const VIDEO_EXTENSIONS = new Set(['.mp4', '.m4v', '.mov']);
const POSTER_EXTENSIONS = ['.jpg', '.jpeg', '.webp', '.png', '.avif'];
const CLIP_OUTPUT_EXTENSION = '.mp4';
const renderJobs = new Map<string, Promise<void>>();

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
    isKeyVideo: Boolean(raw.isKeyVideo),
    label: raw.label ? String(raw.label) : null,
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
    lowResOutputFilePath: raw.lowResOutputFilePath ? normalizeManagedVideoPath(String(raw.lowResOutputFilePath)) : null,
    lowResPaddedOutputFilePath: raw.lowResPaddedOutputFilePath ? normalizeManagedVideoPath(String(raw.lowResPaddedOutputFilePath)) : null,
    publishedAssetId,
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
  clip.publishedLowResFilePath = clip.lowResOutputFilePath;
  clip.publishedLowResPaddedFilePath = clip.lowResPaddedOutputFilePath;
  clip.publishedAt = publishedAt;
  clip.updatedAt = publishedAt;
  return clip;
}

async function bootstrapLegacyMoveAssets(library: VideoLibrary, moves: MoveRecord[]) {
  const moveIds = new Set(moves.map((move) => move.id.toUpperCase()));
  const files = await walkVideoFiles(resolveMediaRoot(), MOVE_VIDEO_PREFIX);
  let changed = false;

  for (const file of files) {
    const normalizedPath = normalizeManagedVideoPath(file.relativePath);
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
    const changed = await bootstrapLegacyMoveAssets(library, moves);
    if (changed) {
      await writeLibraryToDisk(library);
      return structuredClone(library);
    }
  }

  return library;
}

export async function getResolvedMoveVideos(moveId: string, moves: MoveRecord[]) {
  const library = await getVideoLibrary(moves);
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
      moveId
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

async function unlinkIfExists(filePath: string) {
  try {
    await fs.unlink(filePath);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') {
      throw error;
    }
  }
}

async function deleteManagedVideoFiles(filePath: string) {
  await unlinkIfExists(resolveManagedVideoAbsolutePath(filePath));

  await Promise.all(
    POSTER_EXTENSIONS.map((extension) =>
      unlinkIfExists(resolvePathInsideRoot(resolvePosterRoot(), managedPosterPathForVideo(filePath, extension)))
    )
  );
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
  fileBuffer: Buffer;
}) {
  return mutateLibrary(async (library) => {
    await ensureRoots();

    const takenPaths = new Set(
      library.videoAssets
        .filter((asset) => asset.kind === 'source')
        .map((asset) => normalizeManagedVideoPath(asset.filePath).replace(new RegExp(`^${SOURCE_VIDEO_PREFIX}/`), ''))
    );
    const relativeFilePath = uniquePathForDirectory(input.originalFilename, '', takenPaths);
    const absolutePath = resolvePathInsideRoot(resolveSourceRoot(), relativeFilePath);
    await fs.mkdir(path.dirname(absolutePath), { recursive: true });
    await fs.writeFile(absolutePath, input.fileBuffer);

    const asset: VideoAsset = {
      id: randomUUID(),
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
      createdAt: nowIso()
    };

    library.videoAssets.push(asset);
    sortLibrary(library);

    return asset;
  });
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
  return mutateLibrary(async (library) => {
    const asset = library.videoAssets.find((entry) => entry.id === input.assetId && entry.kind === 'source');
    if (!asset) {
      throw new Error('Source asset not found');
    }

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
        clip.label = generatedClipLabel(asset, clip.moveId, index);
      }
      clip.updatedAt = nowIso();
    });
    sortLibrary(library);

    return asset;
  });
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
  return mutateLibrary(async (library) => {
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

    library.derivedClips = library.derivedClips.filter((clip) => clip.sourceAssetId !== sourceAsset.id);
    library.moveVideoLinks = library.moveVideoLinks.filter((link) => !assetIdsToDelete.has(link.assetId));
    library.videoAssets = library.videoAssets.filter((asset) => !assetIdsToDelete.has(asset.id));
    sortLibrary(library);

    await Promise.all([sourceAsset, ...outputAssets].map((asset) => deleteManagedVideoFiles(asset.filePath)));
    await Promise.all(
      sourceClips
        .flatMap((clip) => [
          clip.lowResOutputFilePath,
          clip.lowResPaddedOutputFilePath,
          clip.publishedLowResFilePath,
          clip.publishedLowResPaddedFilePath
        ])
        .filter((filePath): filePath is string => Boolean(filePath))
        .map((filePath) => deleteManagedVideoFiles(filePath))
    );

    return {
      deletedAssetIds,
      deletedClipIds: sourceClips.map((clip) => clip.id)
    };
  });
}

export async function saveSourceClips(input: {
  sourceAssetId: string;
  clips: Array<{
    id?: string;
    moveId: string;
    isKeyVideo?: boolean;
    label?: string | null;
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
  return mutateLibrary(async (library) => {
    const sourceAsset = library.videoAssets.find((asset) => asset.id === input.sourceAssetId && asset.kind === 'source');
    if (!sourceAsset) {
      throw new Error('Source asset not found');
    }

    const existingById = new Map(
      library.derivedClips.filter((clip) => clip.sourceAssetId === input.sourceAssetId).map((clip) => [clip.id, clip])
    );

    const nextClips: DerivedClip[] = input.clips.map((clipInput, index) => {
      const existing = clipInput.id ? existingById.get(clipInput.id) ?? null : null;
      const moveId = clipInput.moveId.trim().toUpperCase();
      const manuallyNamed = Boolean(clipInput.manuallyNamed || (existing?.manuallyNamed && clipInput.label?.trim()));
      const isKeyVideo = clipInput.isKeyVideo === undefined ? existing?.isKeyVideo ?? false : Boolean(clipInput.isKeyVideo);
      const label = manuallyNamed ? clipInput.label?.trim() || existing?.label || null : generatedClipLabel(sourceAsset, moveId, index);
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
      const metadataChanged =
        !existing ||
        existing.isKeyVideo !== isKeyVideo ||
        !countMarkersEqual(existing.countMarkers, countMarkers) ||
        existing.countOverlayPlacement !== countOverlayPlacement ||
        existing.countTimingPreset !== countTimingPreset;
      const changed = renderChanged || metadataChanged || existing?.label !== label || existing?.manuallyNamed !== manuallyNamed;
      const timestamp = changed ? nowIso() : existing.updatedAt;

      return {
        id: existing?.id ?? randomUUID(),
        sourceAssetId: input.sourceAssetId,
        moveId,
        isKeyVideo,
        label,
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
        lowResOutputFilePath: existing?.lowResOutputFilePath ?? null,
        lowResPaddedOutputFilePath: existing?.lowResPaddedOutputFilePath ?? null,
        publishedAssetId: existing?.publishedAssetId ?? null,
        publishedLowResFilePath: existing?.publishedLowResFilePath ?? null,
        publishedLowResPaddedFilePath: existing?.publishedLowResPaddedFilePath ?? null,
        publishedAt: existing?.publishedAt ?? null,
        status: renderChanged ? 'pending' : existing?.status ?? 'pending',
        error: renderChanged ? null : existing?.error ?? null,
        createdAt: existing?.createdAt ?? nowIso(),
        updatedAt: timestamp
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
        clip.lowResOutputFilePath,
        clip.lowResPaddedOutputFilePath,
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
    await Promise.all([
      ...removedAssets.map((asset) => deleteManagedVideoFiles(asset.filePath)),
      ...removedFilePaths.map((filePath) => deleteManagedVideoFiles(filePath))
    ]);
    return nextClips;
  });
}

export async function setClipKeyVideo(input: { clipId: string; isKeyVideo: boolean }) {
  return mutateLibrary(async (library) => {
    const clip = library.derivedClips.find((entry) => entry.id === input.clipId);
    if (!clip) {
      throw new Error('Clip not found');
    }

    clip.isKeyVideo = Boolean(input.isKeyVideo);
    clip.updatedAt = nowIso();
    sortLibrary(library);
    return { ...clip };
  });
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
    'veryfast',
    '-crf',
    lowRes ? '29' : '23'
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

async function renderClip(clipId: string) {
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
  const outputFilename = clipOutputRelativePath(
    clip.moveId,
    sourceAsset.displayName,
    clip.id,
    needsDraftAsset ? `draft ${Date.now().toString(36)}` : ''
  );
  const outputRelativePath = path.posix.join(MOVE_VIDEO_PREFIX, outputFilename);
  const lowResOutputRelativePath = path.posix.join(
    MOVE_VIDEO_PREFIX,
    clipOutputRelativePath(clip.moveId, sourceAsset.displayName, clip.id, `${needsDraftAsset ? `draft ${Date.now().toString(36)} ` : ''}low`)
  );
  const lowResPaddedOutputRelativePath = path.posix.join(
    MOVE_VIDEO_PREFIX,
    clipOutputRelativePath(
      clip.moveId,
      sourceAsset.displayName,
      clip.id,
      `${needsDraftAsset ? `draft ${Date.now().toString(36)} ` : ''}padded low`
    )
  );
  const outputAbsolutePath = resolveManagedVideoAbsolutePath(outputRelativePath);
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
  await renderVideoSegment(inputAbsolutePath, lowResPaddedOutputAbsolutePath, clip.startMs, durationMs, true, clip.cropRect);
  await renderVideoSegment(inputAbsolutePath, lowResOutputAbsolutePath, actionStartMs, actionDurationMs, true, clip.cropRect);

  await mutateLibrary((mutableLibrary) => {
    const mutableClip = mutableLibrary.derivedClips.find((entry) => entry.id === clipId);
    const source = mutableLibrary.videoAssets.find((entry) => entry.id === clip?.sourceAssetId);
    if (!mutableClip || !source) {
      return;
    }

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
    mutableClip.lowResOutputFilePath = lowResOutputRelativePath;
    mutableClip.lowResPaddedOutputFilePath = lowResPaddedOutputRelativePath;
    mutableClip.updatedAt = publishedAt;
    publishClipToMove(mutableLibrary, mutableClip, publishedAt);
    sortLibrary(mutableLibrary);
  });

  void queuePosterGeneration(outputRelativePath);
}

export function queueClipRender(clipId: string) {
  const existing = renderJobs.get(clipId);
  if (existing) {
    return existing;
  }

  const job = renderClip(clipId)
    .catch(async (error) => {
      await mutateLibrary((library) => {
        const clip = library.derivedClips.find((entry) => entry.id === clipId);
        if (clip) {
          clip.status = 'failed';
          clip.error = error instanceof Error ? error.message : 'Render failed';
          clip.updatedAt = nowIso();
        }
      });
    })
    .finally(() => {
      renderJobs.delete(clipId);
    });

  renderJobs.set(clipId, job);
  return job;
}

export function isClipRenderPending(clipId: string) {
  return renderJobs.has(clipId);
}

export async function publishClipsToMoves(clipIds: string[]) {
  const requestedIds = new Set(clipIds.map((clipId) => clipId.trim()).filter(Boolean));
  if (!requestedIds.size) {
    return [];
  }

  return mutateLibrary((library) => {
    const publishedAt = nowIso();
    const publishedClips: DerivedClip[] = [];

    library.derivedClips.forEach((clip) => {
      if (!requestedIds.has(clip.id)) {
        return;
      }

      publishedClips.push({ ...publishClipToMove(library, clip, publishedAt) });
    });

    sortLibrary(library);
    return publishedClips;
  });
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
      pending: isClipRenderPending(clipId)
    };
  });
}

export async function getVideoLibrarySummary(moves: MoveRecord[]) {
  const { assets } = await getUploadPageData(moves);
  return assets;
}
