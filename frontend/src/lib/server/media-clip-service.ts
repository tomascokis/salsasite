import path from 'node:path';
import { randomUUID } from 'node:crypto';
import type {
  ClipCountMarker,
  ClipCropRect,
  CountOverlayPlacement,
  CountTimingPreset,
  DerivedClip,
  VideoLibrary
} from '$lib/types';
import { MOVE_VIDEO_PREFIX } from './paths';
import {
  completeMediaJob,
  createMediaCleanupJob,
  failMediaJob,
  renameManagedVideoFiles,
  startMediaJob,
  trashManagedVideoFileSet
} from './media-manager';
import { mutateMediaCatalog, sortMediaCatalog } from './media-catalog';
import {
  applyDefaultKeyVideoFlags,
  normalizeOptionalText,
  rekeyClipMoveAssociations
} from '$lib/video-library-utils';
import {
  clipOutputRelativePath,
  clipPublicationStatus,
  clipSuffixFromPath,
  countMarkersEqual,
  cropRectsEqual,
  derivedClipAuditSnapshot,
  generatedClipLabel,
  isCountOverlayPlacement,
  isCountTimingPreset,
  moveVideoLinkAuditSnapshot,
  normalizeCountMarkers,
  normalizeCropRect,
  nowIso,
  publishClipToMove,
  recordVideoAuditAction
} from './media-workflow-helpers';

export async function syncDerivedClipDisplayIdForMoveInLibrary(
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

  await mutateMediaCatalog(async (library) => {
    await syncDerivedClipDisplayIdForMoveInLibrary(library, normalizedMoveId, normalizedDisplayId);
    sortMediaCatalog(library);
  });
}

export async function relinkDerivedClipsForPublishedMove(previousMoveId: string, nextMoveId: string, nextMoveDisplayId: string) {
  const normalizedPreviousMoveId = previousMoveId.trim().toUpperCase();
  const normalizedNextMoveId = nextMoveId.trim().toUpperCase();
  const normalizedNextMoveDisplayId = nextMoveDisplayId.trim().toUpperCase();
  if (!normalizedPreviousMoveId || !normalizedNextMoveId || !normalizedNextMoveDisplayId) {
    return;
  }

  await mutateMediaCatalog(async (library) => {
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
      library.moveVideoLinks = relinked.moveVideoLinks;
    }

    await syncDerivedClipDisplayIdForMoveInLibrary(library, normalizedNextMoveId, normalizedNextMoveDisplayId);
    sortMediaCatalog(library);
  });
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
  const result = await mutateMediaCatalog(async (library) => {
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
    sortMediaCatalog(library);
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
  const result = await mutateMediaCatalog(async (library) => {
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
    sortMediaCatalog(library);
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

export async function publishClipsToMoves(clipIds: string[]) {
  const requestedIds = new Set(clipIds.map((clipId) => clipId.trim()).filter(Boolean));
  if (!requestedIds.size) {
    return [];
  }

  const result = await mutateMediaCatalog((library) => {
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

    sortMediaCatalog(library);
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
