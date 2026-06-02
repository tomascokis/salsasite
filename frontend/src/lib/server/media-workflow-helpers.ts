import path from 'node:path';
import { randomUUID } from 'node:crypto';
import type {
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
