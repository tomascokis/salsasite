import fs from 'node:fs/promises';
import path from 'node:path';
import { randomUUID } from 'node:crypto';
import { spawn } from 'node:child_process';
import type { DerivedClip, MoveVideoLink, VideoAsset, VideoOriginType, VideoTiming, VideoContentType, VideoEnvironment } from '$lib/types';
import {
  SOURCE_VIDEO_PREFIX,
  normalizeManagedVideoPath,
  resolveManagedVideoAbsolutePath,
  resolvePathInsideRoot,
  resolveSourceRoot
} from './paths';
import {
  completedMediaJob,
  completeMediaJob,
  createMediaCleanupJob,
  deleteTemporaryFile,
  failMediaJob,
  hashFile,
  listQueuedMediaJobs,
  recordMediaFileAction,
  restoreTrashedFilesForJob,
  startMediaJob,
  trashManagedVideoFileSet,
  upsertMediaJob,
  writeBufferAndHash,
  writeStreamAndHash,
  type MediaFingerprint,
  type MediaJob
} from './media-manager';
import { ensureMediaCatalogRoots, mutateMediaCatalog, readMediaCatalog, sortMediaCatalog } from './media-catalog';
import { normalizeDateString, normalizeOptionalText, normalizeTags } from '$lib/video-library-utils';
import {
  generatedClipLabel,
  nowIso,
  recordVideoAuditAction,
  safeDisplayName,
  sanitizeFilenamePart
} from './media-workflow-helpers';

let sourceHashWorkerRunning = false;

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

function normalizeDancers(dancers: string[] | string) {
  const values = Array.isArray(dancers) ? dancers : dancers.split(',');
  return values.map((value) => value.trim()).filter(Boolean);
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
  const result = await mutateMediaCatalog(async (library) => {
    await ensureMediaCatalogRoots();

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
    sortMediaCatalog(library);

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
  const result = await mutateMediaCatalog(async (library) => {
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
    sortMediaCatalog(library);

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
  const library = await readMediaCatalog();
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
    const result = await mutateMediaCatalog(async (library) => {
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
      sortMediaCatalog(library);

      return {
        deletedAssetIds,
        deletedClipIds
      };
    });

    const capturedActionState = actionState as DeletedSourceMediaSnapshot | null;
    if (capturedActionState) {
      recordVideoAuditAction({
        type: 'media.source.delete',
        label: `Deleted source video: ${capturedActionState.sourceAsset.displayName}`,
        entityType: 'media:source',
        entityId: assetId,
        before: capturedActionState,
        after: {
          mediaJobId: job.id,
          deletedAssetIds: result.deletedAssetIds,
          deletedClipIds: result.deletedClipIds
        }
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

  return mutateMediaCatalog((library) => {
    const assetIds = new Set([sourceAsset.id, ...outputAssets.map((asset) => asset.id)]);
    const clipIds = new Set(sourceClips.map((clip) => clip.id));
    const linkIds = new Set(moveLinks.map((link) => link.id));

    library.videoAssets = library.videoAssets.filter((asset) => !assetIds.has(asset.id));
    library.derivedClips = library.derivedClips.filter((clip) => !clipIds.has(clip.id));
    library.moveVideoLinks = library.moveVideoLinks.filter((link) => !linkIds.has(link.id) && !assetIds.has(link.assetId));
    library.videoAssets.push(structuredClone(sourceAsset), ...structuredClone(outputAssets));
    library.derivedClips.push(...structuredClone(sourceClips));
    library.moveVideoLinks.push(...structuredClone(moveLinks));
    sortMediaCatalog(library);

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
    const library = await readMediaCatalog();
    const sourceAsset = library.videoAssets.find((asset) => asset.id === assetId && asset.kind === 'source');
    if (!sourceAsset) {
      throw new Error('Source asset not found.');
    }

    const absolutePath = resolveManagedVideoAbsolutePath(sourceAsset.filePath);
    const fingerprint = await hashFile(absolutePath);
    await mutateMediaCatalog((mutableLibrary) => {
      const asset = mutableLibrary.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
      if (!asset) {
        throw new Error('Source asset not found.');
      }
      asset.contentHash = fingerprint.contentHash;
      asset.contentHashAlgorithm = fingerprint.contentHashAlgorithm;
      asset.contentSizeBytes = fingerprint.contentSizeBytes;
      asset.hashStatus = 'ready';
      sortMediaCatalog(mutableLibrary);
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
    await mutateMediaCatalog((library) => {
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
  const library = await readMediaCatalog();
  const asset = library.videoAssets.find((entry) => entry.id === assetId && entry.kind === 'source');
  if (!asset) {
    throw new Error('Source asset not found.');
  }

  await mutateMediaCatalog((mutableLibrary) => {
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
  const library = await readMediaCatalog();
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
