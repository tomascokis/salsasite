import path from 'node:path';
import { randomUUID } from 'node:crypto';
import type {
  ClipCountMarker,
  ClipCropRect,
  CountOverlayPlacement,
  CountTimingPreset,
  DerivedClip,
  MoveVideoLink,
  VideoAsset,
  VideoContentType,
  VideoEnvironment,
  VideoLibrary,
  VideoTiming
} from '$lib/types';
import { publicationStatusFor } from '$lib/content-status';
import { recordAction, runInTransaction } from './app-state';

export function nowIso() {
  return new Date().toISOString();
}

export function recordVideoAuditAction(input: {
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

export function sourceAssetAuditSnapshot(asset: VideoAsset | null | undefined) {
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

export function derivedClipAuditSnapshot(clip: DerivedClip | null | undefined) {
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

export function moveVideoLinkAuditSnapshot(link: MoveVideoLink) {
  return {
    id: link.id,
    moveId: link.moveId,
    assetId: link.assetId,
    order: link.order,
    createdAt: link.createdAt
  };
}

export function safeDisplayName(filename: string) {
  return path.parse(filename).name.replace(/[_-]+/g, ' ').trim() || path.parse(filename).name;
}

export function sanitizeFilenamePart(value: string) {
  return value
    .trim()
    .replace(/[\/\\?%*:|"<>]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

export function timingLabel(timing: VideoTiming) {
  if (timing === 'on1') return 'On1';
  if (timing === 'on2') return 'On2';
  return 'Other';
}

export function contentTypeLabel(contentType: VideoContentType) {
  if (contentType === 'music') return 'Music';
  if (contentType === 'counts') return 'Counts';
  return 'Other';
}

export function environmentLabel(environment: VideoEnvironment) {
  if (environment === 'social') return 'Social';
  return 'Class';
}

export function generatedClipLabel(sourceAsset: VideoAsset, moveId: string, index: number) {
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

export function normalizeDancers(dancers: string[] | string) {
  const values = Array.isArray(dancers) ? dancers : dancers.split(',');
  return values.map((value) => value.trim()).filter(Boolean);
}

export function uniquePathForDirectory(fileName: string, relativeDirectory: string, takenPaths: Set<string>) {
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

export function isCountOverlayPlacement(value: unknown): value is CountOverlayPlacement {
  return value === 'top-left' || value === 'top-right' || value === 'bottom-left' || value === 'bottom-right';
}

export function isCountTimingPreset(value: unknown): value is CountTimingPreset {
  return value === 'on2-default' || value === 'on2-all' || value === 'on1-default' || value === 'on1-all';
}

export function normalizeCropRect(value: unknown): ClipCropRect | null {
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

export function normalizeCountMarkers(value: unknown): ClipCountMarker[] {
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

export function cropRectsEqual(left: ClipCropRect | null, right: ClipCropRect | null) {
  if (!left || !right) {
    return left === right;
  }

  return left.x === right.x && left.y === right.y && left.width === right.width && left.height === right.height;
}

export function countMarkersEqual(left: ClipCountMarker[], right: ClipCountMarker[]) {
  if (left.length !== right.length) {
    return false;
  }

  return left.every((marker, index) => {
    const other = right[index];
    return other && marker.count === other.count && marker.ms === other.ms && marker.clear === other.clear;
  });
}

export function ensureMoveLink(library: VideoLibrary, moveId: string, assetId: string) {
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

export function publishClipToMove(library: VideoLibrary, clip: DerivedClip, publishedAt: string) {
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

export function clipPublicationStatus(clip: DerivedClip) {
  return publicationStatusFor({
    isModern: true,
    publishedAt: clip.publishedAt,
    updatedAt: clip.updatedAt
  });
}

export function clipOutputRelativePath(moveId: string, sourceDisplayName: string, clipId: string, suffix = '') {
  const safeDisplay = sanitizeFilenamePart(sourceDisplayName) || 'clip';
  const safeSuffix = sanitizeFilenamePart(suffix);
  return `${moveId} ${safeDisplay} ${clipId.slice(0, 8)}${safeSuffix ? ` ${safeSuffix}` : ''}.mp4`;
}

export function clipSuffixFromPath(filePath: string | null, clipId: string) {
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
