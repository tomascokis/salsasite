import fs from 'node:fs/promises';
import type { Dirent } from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';
import type { DerivedClip, MoveRecord, MoveVideoLink, VideoLibrary } from '$lib/types';
import {
  MOVE_VIDEO_PREFIX,
  normalizeManagedVideoPath,
  resolveManagedVideoAbsolutePath,
  resolveMediaRoot
} from './paths';
import {
  completeMediaJob,
  createMediaCleanupJob,
  failMediaJob,
  startMediaJob,
  trashManagedVideoFileSet
} from './media-manager';
import {
  generatedDerivedClipFileInfo,
  generatedDerivedClipFileMatches,
  rekeyClipMoveAssociations
} from '$lib/video-library-utils';
import { moveDisplayId } from '$lib/move-id';
import { generatedMoveIdStem } from '$lib/move-id-utils.js';
import { listMoveDrafts } from './move-editor';
import {
  readMediaCatalog,
  sortMediaCatalog,
  writeMediaCatalog
} from './media-catalog';
import { syncDerivedClipDisplayIdForMoveInLibrary } from './media-clip-service';
import {
  ensureMoveLink,
  safeDisplayName,
  sanitizeFilenamePart
} from './media-workflow-helpers';

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
