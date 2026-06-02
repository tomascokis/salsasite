import fs from 'node:fs/promises';
import type { Dirent } from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';
import type { DerivedClip, MoveRecord, MoveVideoLink, VideoAsset, VideoLibrary } from '$lib/types';
import { moveDisplayId } from '$lib/move-id';
import { generatedMoveIdStem } from '$lib/move-id-utils.js';
import {
  generatedDerivedClipFileInfo,
  generatedDerivedClipFileMatches,
  rekeyClipMoveAssociations
} from '$lib/video-library-utils';
import {
  MOVE_VIDEO_PREFIX,
  normalizeManagedVideoPath,
  resolveManagedVideoAbsolutePath,
  resolveMediaRoot
} from './paths';
import { readMediaCatalog, mutateMediaCatalog, sortMediaCatalog } from './media-catalog';
import {
  completeMediaJob,
  createMediaCleanupJob,
  failMediaJob,
  renameManagedVideoFiles,
  startMediaJob,
  trashManagedVideoFileSet
} from './media-manager';
import { listMoveDrafts } from './move-editor';
import {
  applyDerivedClipDisplayIdForMoveCatalogChanges,
  type ManagedVideoRenamePlan
} from './media-clip-service';
import {
  ensureMoveLink,
  recordVideoAuditAction,
  safeDisplayName,
  sanitizeFilenamePart
} from './media-workflow-helpers';

const VIDEO_EXTENSIONS = new Set(['.mp4', '.m4v', '.mov']);
const VARIANT_KEYS = [
  'actionOutputFilePath',
  'lowResOutputFilePath',
  'lowResPaddedOutputFilePath',
  'publishedActionOutputFilePath',
  'publishedLowResFilePath',
  'publishedLowResPaddedFilePath'
] as const;

type VariantKey = typeof VARIANT_KEYS[number];

export type MediaCatalogRepairSummary = {
  missingVariantPathsPruned: number;
  legacyMoveAssetsAdded: number;
  legacyLinksAdded: number;
  staleGeneratedAssetsRemoved: number;
  staleGeneratedFilesTrashed: number;
  orphanDraftMoveIdsRelinked: number;
  generatedRenameActions: number;
  generatedVariantLinksRemoved: number;
};

export type MediaCatalogRepairResult = {
  dryRun: boolean;
  changed: boolean;
  summary: MediaCatalogRepairSummary;
  changes: {
    missingVariantPathsPruned: Array<{ clipId: string; key: VariantKey; filePath: string }>;
    legacyMoveAssetsAdded: Array<{ assetId: string; filePath: string; originalFilename: string }>;
    legacyLinksAdded: Array<{ moveId: string; assetId: string; filePath: string }>;
    staleGeneratedAssetsRemoved: Array<{ assetId: string; filePath: string }>;
    staleGeneratedFilesTrashed: string[];
    orphanDraftMoveIdsRelinked: Array<{
      fromMoveId: string;
      toMoveId: string;
      toMoveDisplayId: string;
      derivedClipIds: string[];
      moveVideoLinkIds: string[];
    }>;
    generatedRenameActions: Array<ManagedVideoRenamePlan & { moveId: string; nextMoveDisplayId: string }>;
    generatedVariantLinksRemoved: Array<{ linkId: string; moveId: string; assetId: string }>;
  };
  mediaJobIds: string[];
  historyActionId?: string;
};

type RepairContext = {
  result: MediaCatalogRepairResult;
  apply: boolean;
  mediaJobIds: string[];
};

function emptySummary(): MediaCatalogRepairSummary {
  return {
    missingVariantPathsPruned: 0,
    legacyMoveAssetsAdded: 0,
    legacyLinksAdded: 0,
    staleGeneratedAssetsRemoved: 0,
    staleGeneratedFilesTrashed: 0,
    orphanDraftMoveIdsRelinked: 0,
    generatedRenameActions: 0,
    generatedVariantLinksRemoved: 0
  };
}

function emptyRepairResult(dryRun: boolean): MediaCatalogRepairResult {
  return {
    dryRun,
    changed: false,
    summary: emptySummary(),
    changes: {
      missingVariantPathsPruned: [],
      legacyMoveAssetsAdded: [],
      legacyLinksAdded: [],
      staleGeneratedAssetsRemoved: [],
      staleGeneratedFilesTrashed: [],
      orphanDraftMoveIdsRelinked: [],
      generatedRenameActions: [],
      generatedVariantLinksRemoved: []
    },
    mediaJobIds: []
  };
}

function markChanged(result: MediaCatalogRepairResult) {
  result.changed = true;
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

function derivedClipVariantPaths(library: VideoLibrary) {
  const paths = new Set<string>();
  for (const clip of library.derivedClips) {
    VARIANT_KEYS.forEach((key) => {
      const filePath = clip[key];
      if (filePath) {
        paths.add(normalizeManagedVideoPath(filePath));
      }
    });
  }
  return paths;
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

function catalogCounts(library: VideoLibrary) {
  return {
    videoAssets: library.videoAssets.length,
    moveVideoLinks: library.moveVideoLinks.length,
    derivedClips: library.derivedClips.length
  };
}

async function pruneMissingDerivedVariantPaths(library: VideoLibrary, context: RepairContext) {
  for (const clip of library.derivedClips) {
    for (const key of VARIANT_KEYS) {
      const filePath = clip[key];
      if (!filePath || (await managedVideoFileExists(filePath))) {
        continue;
      }

      context.result.summary.missingVariantPathsPruned += 1;
      context.result.changes.missingVariantPathsPruned.push({ clipId: clip.id, key, filePath });
      markChanged(context.result);
      if (context.apply) {
        clip[key] = null;
      }
    }
  }
}

function legacyMoveAssetFromFile(file: { relativePath: string; originalFilename: string; createdAt: string }): VideoAsset {
  const normalizedPath = normalizeManagedVideoPath(file.relativePath);
  return {
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
}

async function cleanupStaleGeneratedAssets(library: VideoLibrary, staleGeneratedAssets: VideoAsset[], context: RepairContext) {
  if (!staleGeneratedAssets.length) {
    return;
  }

  const filePaths = staleGeneratedAssets.map((asset) => asset.filePath);
  context.result.summary.staleGeneratedFilesTrashed += filePaths.length;
  context.result.changes.staleGeneratedFilesTrashed.push(...filePaths);

  if (!context.apply) {
    return;
  }

  const cleanupJob = createMediaCleanupJob({
    targetType: 'legacyGeneratedAssets',
    targetId: 'catalog-repair',
    payload: {
      assetIds: staleGeneratedAssets.map((asset) => asset.id),
      filePaths
    }
  });
  context.mediaJobIds.push(cleanupJob.id);
  startMediaJob(cleanupJob.id);
  try {
    await trashManagedVideoFileSet({
      jobId: cleanupJob.id,
      filePaths,
      actionType: 'cleanup-generated',
      metadata: { cleanupReason: 'legacy-generated-bootstrap' }
    });
    completeMediaJob(cleanupJob.id);
  } catch (error) {
    failMediaJob(cleanupJob.id, error);
    throw error;
  }
}

async function repairLegacyMoveAssets(library: VideoLibrary, moves: MoveRecord[], context: RepairContext) {
  const moveIds = new Set(moves.map((move) => move.id.toUpperCase()));
  const variantPaths = derivedClipVariantPaths(library);
  const outputAssetIds = derivedClipOutputAssetIds(library);
  const linkedAssetIds = new Set(library.moveVideoLinks.map((link) => link.assetId));
  const variantAssetIds = new Set(
    library.videoAssets
      .filter((asset) => asset.kind === 'move' && variantPaths.has(normalizeManagedVideoPath(asset.filePath)))
      .map((asset) => asset.id)
  );
  const staleGeneratedAssets = library.videoAssets.filter((asset) => {
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
  });
  const staleGeneratedAssetIds = new Set(staleGeneratedAssets.map((asset) => asset.id));
  const assetIdsToUnlink = new Set([...variantAssetIds, ...staleGeneratedAssetIds]);
  const removedLinks = library.moveVideoLinks.filter((link) => assetIdsToUnlink.has(link.assetId));

  for (const asset of staleGeneratedAssets) {
    context.result.summary.staleGeneratedAssetsRemoved += 1;
    context.result.changes.staleGeneratedAssetsRemoved.push({ assetId: asset.id, filePath: asset.filePath });
    markChanged(context.result);
  }

  for (const link of removedLinks) {
    context.result.summary.generatedVariantLinksRemoved += 1;
    context.result.changes.generatedVariantLinksRemoved.push({
      linkId: link.id,
      moveId: link.moveId,
      assetId: link.assetId
    });
    markChanged(context.result);
  }

  if (context.apply && removedLinks.length) {
    library.moveVideoLinks = library.moveVideoLinks.filter((link) => !assetIdsToUnlink.has(link.assetId));
  }
  if (context.apply && staleGeneratedAssets.length) {
    library.videoAssets = library.videoAssets.filter((asset) => !staleGeneratedAssetIds.has(asset.id));
  }
  await cleanupStaleGeneratedAssets(library, staleGeneratedAssets, context);

  const files = await walkVideoFiles(resolveMediaRoot(), MOVE_VIDEO_PREFIX);
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
      asset = legacyMoveAssetFromFile(file);
      context.result.summary.legacyMoveAssetsAdded += 1;
      context.result.changes.legacyMoveAssetsAdded.push({
        assetId: asset.id,
        filePath: asset.filePath,
        originalFilename: asset.originalFilename
      });
      markChanged(context.result);
      if (context.apply) {
        library.videoAssets.push(asset);
      }
    }

    const moveId = extractMoveIdFromFilename(file.originalFilename);
    if (!moveId || !moveIds.has(moveId)) {
      continue;
    }

    const existing = library.moveVideoLinks.find((entry) => entry.moveId === moveId && entry.assetId === asset.id);
    if (existing) {
      continue;
    }

    context.result.summary.legacyLinksAdded += 1;
    context.result.changes.legacyLinksAdded.push({ moveId, assetId: asset.id, filePath: asset.filePath });
    markChanged(context.result);
    if (context.apply) {
      ensureMoveLink(library, moveId, asset.id);
    }
  }
}

async function applyGeneratedRenames(
  moveId: string,
  nextMoveDisplayId: string,
  renames: ManagedVideoRenamePlan[],
  context: RepairContext
) {
  if (!context.apply || !renames.length) {
    return;
  }

  const renameJob = createMediaCleanupJob({
    targetType: 'move',
    targetId: moveId,
    payload: {
      nextMoveDisplayId,
      renames
    }
  });
  context.mediaJobIds.push(renameJob.id);
  startMediaJob(renameJob.id);
  try {
    for (const { fromPath, toPath } of renames) {
      await renameManagedVideoFiles({
        jobId: renameJob.id,
        fromPath,
        toPath,
        metadata: {
          moveId,
          nextMoveDisplayId
        }
      });
    }
    completeMediaJob(renameJob.id);
  } catch (error) {
    failMediaJob(renameJob.id, error);
    throw error;
  }
}

async function relinkOrphanedGeneratedDraftMoveIds(library: VideoLibrary, moves: MoveRecord[], context: RepairContext) {
  const publishedMoveIds = new Set(moves.map((move) => move.id.toUpperCase()));
  const activeDraftIds = new Set((await listMoveDrafts()).map((draft) => draft.move.id.toUpperCase()));
  const aliasCandidates = new Map<string, MoveRecord | null>();

  for (const move of moves) {
    const generatedDraftId = generatedMoveIdStem(move.name ?? '');
    const normalizedMoveId = move.id.toUpperCase();
    if (
      !generatedDraftId ||
      generatedDraftId === normalizedMoveId ||
      publishedMoveIds.has(generatedDraftId) ||
      activeDraftIds.has(generatedDraftId)
    ) {
      continue;
    }

    aliasCandidates.set(generatedDraftId, aliasCandidates.has(generatedDraftId) ? null : move);
  }

  for (const [draftMoveId, move] of aliasCandidates.entries()) {
    if (!move) {
      continue;
    }

    const orphanedClips = library.derivedClips.filter(
      (clip) => String(clip.moveId ?? '').trim().toUpperCase() === draftMoveId
    );
    const orphanedLinks = library.moveVideoLinks.filter(
      (link) => String(link.moveId ?? '').trim().toUpperCase() === draftMoveId
    );
    if (!orphanedClips.length && !orphanedLinks.length) {
      continue;
    }

    const nextDisplayId = moveDisplayId(move);
    const relinked = rekeyClipMoveAssociations(library, draftMoveId, move.id, nextDisplayId);
    if (!relinked.changed) {
      continue;
    }

    library.derivedClips = relinked.derivedClips as DerivedClip[];
    library.moveVideoLinks = relinked.moveVideoLinks as MoveVideoLink[];
    const renames = applyDerivedClipDisplayIdForMoveCatalogChanges(library, move.id, nextDisplayId);

    context.result.summary.orphanDraftMoveIdsRelinked += 1;
    context.result.summary.generatedRenameActions += renames.length;
    context.result.changes.orphanDraftMoveIdsRelinked.push({
      fromMoveId: draftMoveId,
      toMoveId: move.id,
      toMoveDisplayId: nextDisplayId,
      derivedClipIds: orphanedClips.map((clip) => clip.id),
      moveVideoLinkIds: orphanedLinks.map((link) => link.id)
    });
    context.result.changes.generatedRenameActions.push(
      ...renames.map((rename) => ({
        ...rename,
        moveId: move.id,
        nextMoveDisplayId: nextDisplayId
      }))
    );
    markChanged(context.result);
    await applyGeneratedRenames(move.id, nextDisplayId, renames, context);
  }
}

async function repairCatalogInMemory(library: VideoLibrary, moves: MoveRecord[], context: RepairContext) {
  await pruneMissingDerivedVariantPaths(library, context);
  await repairLegacyMoveAssets(library, moves, context);
  await relinkOrphanedGeneratedDraftMoveIds(library, moves, context);
  if (context.apply && context.result.changed) {
    sortMediaCatalog(library);
  }
}

async function buildRepairResult(library: VideoLibrary, moves: MoveRecord[], dryRun: boolean) {
  const result = emptyRepairResult(dryRun);
  const mediaJobIds: string[] = [];
  const context = {
    result,
    apply: !dryRun,
    mediaJobIds
  };
  await repairCatalogInMemory(library, moves, context);
  result.mediaJobIds = mediaJobIds;
  return result;
}

export async function scanMediaCatalogRepairs(moves: MoveRecord[]) {
  const library = await readMediaCatalog();
  return buildRepairResult(library, moves, true);
}

export async function runMediaCatalogRepairs(moves: MoveRecord[], options: { audit?: boolean } = {}) {
  const initialScan = await scanMediaCatalogRepairs(moves);
  if (!initialScan.changed) {
    return {
      ...initialScan,
      dryRun: false
    };
  }

  let beforeCounts = catalogCounts(await readMediaCatalog());
  let afterCounts = beforeCounts;
  const result = await mutateMediaCatalog(async (library) => {
    beforeCounts = catalogCounts(library);
    const repair = await buildRepairResult(library, moves, false);
    afterCounts = catalogCounts(library);
    return repair;
  });

  if (result.changed && options.audit !== false) {
    const action = recordVideoAuditAction({
      type: 'media.catalog.repair',
      label: 'Repaired media catalog',
      entityType: 'media:catalog',
      entityId: 'video-library',
      before: {
        counts: beforeCounts
      },
      after: {
        counts: afterCounts,
        summary: result.summary,
        mediaJobIds: result.mediaJobIds
      }
    });
    result.historyActionId = action.id;
  }

  return result;
}
