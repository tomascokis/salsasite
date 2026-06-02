import type { DerivedClip, MoveRecord, MoveVideoEntry, VideoAsset, VideoEnvironment } from '$lib/types';
import { normalizeManagedVideoPath } from './paths';
import { findPosterForVideoFile } from './posters';
import { getPositionOptions, positionLabelById } from './positions';
import { sourceSuggestions, uploadMonthKey } from '$lib/video-library-utils';
import { readVideoLibrary } from './media-bootstrap-service';
import { isClipRenderPending } from './media-render-service';
import {
  clipPublicationStatus,
  contentTypeLabel,
  environmentLabel,
  timingLabel
} from './media-workflow-helpers';

export async function getResolvedMoveVideos(moveId: string, moves: MoveRecord[]) {
  const library = await readVideoLibrary();
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
  const library = await readVideoLibrary();
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

export async function getSourceAssets(moves?: MoveRecord[]) {
  const library = await readVideoLibrary();
  return library.videoAssets.filter((asset) => asset.kind === 'source');
}

export async function getUploadPageData(moves: MoveRecord[]) {
  const library = await readVideoLibrary();
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
  const library = await readVideoLibrary();
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
  const library = await readVideoLibrary();
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
