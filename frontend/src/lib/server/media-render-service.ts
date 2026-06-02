import fs from 'node:fs/promises';
import path from 'node:path';
import { randomUUID } from 'node:crypto';
import { spawn } from 'node:child_process';
import type { ClipCropRect } from '$lib/types';
import { MOVE_VIDEO_PREFIX, resolveManagedVideoAbsolutePath } from './paths';
import { queuePosterGeneration } from './posters';
import {
  completeMediaJob,
  failMediaJob,
  isMediaJobTargetPending,
  listQueuedMediaJobs,
  recordMediaFileAction,
  startMediaJob,
  trashManagedVideoFileSet,
  upsertMediaJob,
  type MediaJob
} from './media-manager';
import { mutateMediaCatalog, readMediaCatalog, sortMediaCatalog } from './media-catalog';
import { obsoleteGeneratedClipFilePaths } from '$lib/video-library-utils';
import {
  clipOutputRelativePath,
  nowIso,
  publishClipToMove,
  recordVideoAuditAction
} from './media-workflow-helpers';

const FULL_QUALITY_CRF = '18';
const LOW_QUALITY_CRF = '29';
let renderWorkerRunning = false;

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
  const library = await readMediaCatalog();
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

  await mutateMediaCatalog((mutableLibrary) => {
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
  await mutateMediaCatalog((mutableLibrary) => {
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
    sortMediaCatalog(mutableLibrary);
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
    await mutateMediaCatalog((library) => {
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
