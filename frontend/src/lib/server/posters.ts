import fs from 'node:fs/promises';
import path from 'node:path';
import { spawn } from 'node:child_process';
import {
  managedPosterPathForVideo,
  normalizeManagedVideoPath,
  resolveManagedVideoAbsolutePath,
  resolvePathInsideRoot,
  resolvePosterRoot
} from './paths';
import {
  completeMediaJob,
  failMediaJob,
  isMediaJobPending,
  recordMediaFileAction,
  startMediaJob,
  upsertMediaJob
} from './media-manager';

const POSTER_EXTENSIONS = ['.jpg', '.jpeg', '.webp', '.png', '.avif'];
const DEFAULT_POSTER_EXTENSION = '.jpg';
const DEFAULT_POSTER_TIMESTAMP_SECONDS = process.env.POSTER_TIMESTAMP_SECONDS ?? '1.0';

const posterJobs = new Map<string, Promise<string | null>>();
let posterGeneratorUnavailable = false;

function resolveVideoAbsolutePath(videoFile: string) {
  return resolveManagedVideoAbsolutePath(videoFile);
}

function resolvePosterPath(videoFile: string, extension = DEFAULT_POSTER_EXTENSION) {
  const relativePosterPath = managedPosterPathForVideo(videoFile, extension);
  const absolutePosterPath = resolvePathInsideRoot(resolvePosterRoot(), relativePosterPath);

  return {
    relativePosterPath,
    absolutePosterPath
  };
}

export async function findPosterForVideoFile(videoFile: string) {
  try {
    for (const extension of POSTER_EXTENSIONS) {
      const { relativePosterPath, absolutePosterPath } = resolvePosterPath(videoFile, extension);

      try {
        await fs.access(absolutePosterPath);
        return relativePosterPath;
      } catch {
        // Try the next poster extension.
      }
    }
  } catch {
    return null;
  }

  return null;
}

async function generatePosterForVideoFile(videoFile: string) {
  const existingPoster = await findPosterForVideoFile(videoFile);
  if (existingPoster) {
    return existingPoster;
  }

  let inputPath: string;
  let posterPath: ReturnType<typeof resolvePosterPath>;

  try {
    inputPath = resolveVideoAbsolutePath(videoFile);
    posterPath = resolvePosterPath(videoFile);
  } catch {
    return null;
  }

  await fs.mkdir(path.dirname(posterPath.absolutePosterPath), { recursive: true });

  return await new Promise<string | null>((resolve) => {
    const ffmpeg = spawn(
      'ffmpeg',
      [
        '-hide_banner',
        '-loglevel',
        'error',
        '-y',
        '-ss',
        DEFAULT_POSTER_TIMESTAMP_SECONDS,
        '-i',
        inputPath,
        '-frames:v',
        '1',
        '-q:v',
        '2',
        posterPath.absolutePosterPath
      ],
      { stdio: 'ignore' }
    );

    ffmpeg.once('error', () => {
      posterGeneratorUnavailable = true;
      resolve(null);
    });

    ffmpeg.once('close', async (code) => {
      if (code !== 0) {
        resolve(null);
        return;
      }

      try {
        await fs.access(posterPath.absolutePosterPath);
        resolve(posterPath.relativePosterPath);
      } catch {
        resolve(null);
      }
    });
  });
}

export function queuePosterGeneration(videoFile: string) {
  if (posterGeneratorUnavailable) {
    return Promise.resolve(null);
  }

  const normalizedVideoFile = normalizeManagedVideoPath(videoFile);
  const idempotencyKey = `poster.generate:${normalizedVideoFile}`;
  const existingJob = posterJobs.get(normalizedVideoFile);
  if (existingJob) {
    return existingJob;
  }

  const jobRecord = upsertMediaJob({
    type: 'poster.generate',
    targetType: 'managedVideo',
    targetId: normalizedVideoFile,
    idempotencyKey,
    payload: { videoFile: normalizedVideoFile },
    retryFailed: true,
    retryCompleted: true
  });

  const job = (async () => {
    const existingPoster = await findPosterForVideoFile(normalizedVideoFile);
    if (existingPoster) {
      if (jobRecord.status !== 'succeeded') {
        startMediaJob(jobRecord.id);
        completeMediaJob(jobRecord.id);
      }
      return existingPoster;
    }

    try {
      startMediaJob(jobRecord.id);
      const posterFile = await generatePosterForVideoFile(normalizedVideoFile);
      if (!posterFile) {
        throw new Error('Poster generation did not create an output file.');
      }

      completeMediaJob(jobRecord.id);
      recordMediaFileAction({
        jobId: jobRecord.id,
        actionType: 'generate-poster',
        status: 'succeeded',
        filePath: posterFile,
        metadata: { videoFile: normalizedVideoFile }
      });
      return posterFile;
    } catch (error) {
      failMediaJob(jobRecord.id, error);
      return null;
    }
  })().finally(() => {
    posterJobs.delete(normalizedVideoFile);
  });

  posterJobs.set(normalizedVideoFile, job);
  return job;
}

export function isPosterGenerationPending(videoFile: string) {
  const normalizedVideoFile = normalizeManagedVideoPath(videoFile);
  return posterJobs.has(normalizedVideoFile) || isMediaJobPending(`poster.generate:${normalizedVideoFile}`);
}

export function isPosterGeneratorUnavailable() {
  return posterGeneratorUnavailable;
}
