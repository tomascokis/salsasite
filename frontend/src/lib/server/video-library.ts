import fs from 'node:fs/promises';
import type { Dirent } from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';
import type {
  DerivedClip,
  MoveRecord,
  MoveVideoEntry,
  MoveVideoLink,
  VideoAsset,
  VideoEnvironment,
  VideoLibrary
} from '$lib/types';
import {
  MOVE_VIDEO_PREFIX,
  normalizeManagedVideoPath,
  resolveManagedVideoAbsolutePath,
  resolveMediaRoot
} from './paths';
import { findPosterForVideoFile, queuePosterGeneration } from './posters';
import {
  completeMediaJob,
  createMediaCleanupJob,
  failMediaJob,
  getMediaJobById,
  listMediaJobsWithFileActions,
  startMediaJob,
  trashManagedVideoFileSet
} from './media-manager';
import { getPositionOptions, positionLabelById } from './positions';
import {
  generatedDerivedClipFileInfo,
  generatedDerivedClipFileMatches,
  rekeyClipMoveAssociations,
  sourceSuggestions,
  uploadMonthKey
} from '$lib/video-library-utils';
import { moveDisplayId } from '$lib/move-id';
import { generatedMoveIdStem } from '$lib/move-id-utils.js';
import { listMoveDrafts } from './move-editor';
import {
  mutateMediaCatalog,
  readMediaCatalog,
  sortMediaCatalog,
  writeMediaCatalog
} from './media-catalog';
import {
  queueSourceHash,
  queueSourceHashBackfill
} from './media-source-service';
import { syncDerivedClipDisplayIdForMoveInLibrary } from './media-clip-service';
import {
  isClipRenderPending,
  queueClipRender
} from './media-render-service';
import {
  clipPublicationStatus,
  contentTypeLabel,
  ensureMoveLink,
  environmentLabel,
  safeDisplayName,
  sanitizeFilenamePart,
  timingLabel
} from './media-workflow-helpers';

export {
  createSourceAsset,
  deleteSourceAsset,
  detectSourceAssetFields,
  queueSourceHash,
  queueSourceHashBackfill,
  restoreDeletedSourceMedia,
  updateSourceAsset
} from './media-source-service';
export {
  publishClipsToMoves,
  relinkDerivedClipsForPublishedMove,
  saveSourceClips,
  setClipKeyVideo,
  syncDerivedClipDisplayIdForMove
} from './media-clip-service';
export {
  cleanupObsoleteRenderedClipFiles,
  isClipRenderPending,
  queueClipRender
} from './media-render-service';

const VIDEO_EXTENSIONS = new Set(['.mp4', '.m4v', '.mov']);

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
  const library = await readMediaCatalog();

  if (moves) {
    let changed = await pruneMissingDerivedVariantPaths(library);
    changed = (await bootstrapLegacyMoveAssets(library, moves)) || changed;
    changed = (await relinkOrphanedGeneratedDraftMoveIds(library, moves)) || changed;
    if (changed) {
      await writeMediaCatalog(library);
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
    sortMediaCatalog(library);
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
  const library = await readMediaCatalog();
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
