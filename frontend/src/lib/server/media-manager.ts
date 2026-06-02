import fs from 'node:fs';
import fsp from 'node:fs/promises';
import path from 'node:path';
import { randomUUID, createHash } from 'node:crypto';
import { Transform, Readable } from 'node:stream';
import { pipeline } from 'node:stream/promises';
import { getAppDatabase, runInTransaction } from './app-state';
import {
  managedPosterPathForVideo,
  normalizeManagedVideoPath,
  resolveDataDir,
  resolveManagedVideoAbsolutePath,
  resolvePathInsideRoot,
  resolvePosterRoot
} from './paths';

export type MediaJobStatus = 'queued' | 'running' | 'succeeded' | 'failed' | 'cancelled';
export type MediaJobType = 'source.hash' | 'poster.generate' | 'clip.render' | 'file.cleanup' | 'file.delete';
export type MediaHashStatus = 'pending' | 'ready' | 'failed';
export type MediaHashAlgorithm = 'sha256';

export type MediaFingerprint = {
  contentHash: string;
  contentHashAlgorithm: MediaHashAlgorithm;
  contentSizeBytes: number;
};

export type MediaJob = {
  id: string;
  type: string;
  targetType: string;
  targetId: string;
  status: MediaJobStatus;
  idempotencyKey: string;
  payload: unknown;
  attempts: number;
  maxAttempts: number;
  error: string | null;
  createdAt: string;
  updatedAt: string;
  startedAt: string | null;
  finishedAt: string | null;
};

export type MediaFileAction = {
  id: string;
  jobId: string;
  actionType: string;
  status: 'planned' | 'running' | 'succeeded' | 'failed';
  filePath: string;
  backupPath: string | null;
  metadata: unknown;
  createdAt: string;
  updatedAt: string;
};

export type MediaFileActionSummary = {
  total: number;
  failed: number;
  missing: number;
  byActionType: Record<string, number>;
};

export type MediaJobWithFileActions = MediaJob & {
  fileActions: MediaFileAction[];
  fileActionSummary: MediaFileActionSummary;
};

type MediaJobRow = {
  id: string;
  type: string;
  target_type: string;
  target_id: string;
  status: string;
  idempotency_key: string;
  payload_json: string;
  attempts: number;
  max_attempts: number;
  error: string | null;
  created_at: string;
  updated_at: string;
  started_at: string | null;
  finished_at: string | null;
};

type MediaFileActionRow = {
  id: string;
  job_id: string;
  action_type: string;
  status: string;
  file_path: string;
  backup_path: string | null;
  metadata_json: string;
  created_at: string;
  updated_at: string;
};

let recoveredInterruptedJobs = false;

function nowIso() {
  return new Date().toISOString();
}

function serializeJson(value: unknown) {
  return JSON.stringify(value ?? null);
}

function parseJson(value: string) {
  return JSON.parse(value);
}

function mediaJobFromRow(row: MediaJobRow): MediaJob {
  return {
    id: row.id,
    type: row.type,
    targetType: row.target_type,
    targetId: row.target_id,
    status: isMediaJobStatus(row.status) ? row.status : 'failed',
    idempotencyKey: row.idempotency_key,
    payload: parseJson(row.payload_json),
    attempts: Number(row.attempts),
    maxAttempts: Number(row.max_attempts),
    error: row.error,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    startedAt: row.started_at,
    finishedAt: row.finished_at
  };
}

function mediaFileActionFromRow(row: MediaFileActionRow): MediaFileAction {
  return {
    id: row.id,
    jobId: row.job_id,
    actionType: row.action_type,
    status:
      row.status === 'planned' || row.status === 'running' || row.status === 'succeeded' || row.status === 'failed'
        ? row.status
        : 'failed',
    filePath: row.file_path,
    backupPath: row.backup_path,
    metadata: parseJson(row.metadata_json),
    createdAt: row.created_at,
    updatedAt: row.updated_at
  };
}

function sanitizeMediaMetadata(value: unknown): unknown {
  if (Array.isArray(value)) {
    return value.map(sanitizeMediaMetadata);
  }

  if (!value || typeof value !== 'object') {
    return value;
  }

  return Object.fromEntries(
    Object.entries(value as Record<string, unknown>)
      .filter(([key]) => key !== 'originalAbsolutePath' && key !== 'destinationAbsolutePath')
      .map(([key, entry]) => [key, sanitizeMediaMetadata(entry)])
  );
}

function isMissingFileAction(action: MediaFileAction) {
  return Boolean(
    action.metadata &&
      typeof action.metadata === 'object' &&
      (action.metadata as Record<string, unknown>).missing === true
  );
}

function summarizeMediaFileActions(fileActions: MediaFileAction[]): MediaFileActionSummary {
  const byActionType: Record<string, number> = {};
  let failed = 0;
  let missing = 0;

  for (const action of fileActions) {
    byActionType[action.actionType] = (byActionType[action.actionType] ?? 0) + 1;
    if (action.status === 'failed') {
      failed += 1;
    }
    if (isMissingFileAction(action)) {
      missing += 1;
    }
  }

  return {
    total: fileActions.length,
    failed,
    missing,
    byActionType
  };
}

function isMediaJobStatus(value: string): value is MediaJobStatus {
  return value === 'queued' || value === 'running' || value === 'succeeded' || value === 'failed' || value === 'cancelled';
}

function recoverInterruptedJobs() {
  if (recoveredInterruptedJobs) {
    return;
  }

  const recoveredAt = nowIso();
  getAppDatabase()
    .prepare(
      `
        UPDATE media_jobs
        SET status = ?, updated_at = ?, error = ?
        WHERE status = ?
      `
    )
    .run('queued', recoveredAt, 'Recovered after server restart before completion.', 'running');
  recoveredInterruptedJobs = true;
}

export function getMediaJobByIdempotencyKey(idempotencyKey: string) {
  recoverInterruptedJobs();
  const row = getAppDatabase()
    .prepare('SELECT * FROM media_jobs WHERE idempotency_key = ?')
    .get(idempotencyKey) as MediaJobRow | undefined;
  return row ? mediaJobFromRow(row) : null;
}

export function getMediaJobById(id: string) {
  recoverInterruptedJobs();
  const row = getAppDatabase().prepare('SELECT * FROM media_jobs WHERE id = ?').get(id) as MediaJobRow | undefined;
  return row ? mediaJobFromRow(row) : null;
}

export function getMediaJobByTarget(type: MediaJobType, targetType: string, targetId: string) {
  recoverInterruptedJobs();
  const row = getAppDatabase()
    .prepare(
      `
        SELECT * FROM media_jobs
        WHERE type = ? AND target_type = ? AND target_id = ?
        ORDER BY created_at DESC
        LIMIT 1
      `
    )
    .get(type, targetType, targetId) as MediaJobRow | undefined;
  return row ? mediaJobFromRow(row) : null;
}

export function listQueuedMediaJobs(type: MediaJobType, limit = 10) {
  recoverInterruptedJobs();
  const rows = getAppDatabase()
    .prepare(
      `
        SELECT * FROM media_jobs
        WHERE type = ? AND status = ?
        ORDER BY created_at ASC
        LIMIT ?
      `
    )
    .all(type, 'queued', Math.max(1, Math.min(50, Math.floor(limit)))) as MediaJobRow[];
  return rows.map(mediaJobFromRow);
}

export function listMediaJobs(limit = 100) {
  recoverInterruptedJobs();
  const rows = getAppDatabase()
    .prepare(
      `
        SELECT * FROM media_jobs
        ORDER BY updated_at DESC
        LIMIT ?
      `
    )
    .all(Math.max(1, Math.min(250, Math.floor(limit)))) as MediaJobRow[];
  return rows.map(mediaJobFromRow);
}

export function listMediaJobsWithFileActions(limit = 100): MediaJobWithFileActions[] {
  return listMediaJobs(limit).map((job) => {
    const fileActions = listMediaFileActionsForJob(job.id).map((action) => ({
      ...action,
      metadata: sanitizeMediaMetadata(action.metadata)
    }));

    return {
      ...job,
      fileActions,
      fileActionSummary: summarizeMediaFileActions(fileActions)
    };
  });
}

export function upsertMediaJob(input: {
  type: MediaJobType;
  targetType: string;
  targetId: string;
  idempotencyKey: string;
  payload?: unknown;
  maxAttempts?: number;
  retryFailed?: boolean;
  retryCompleted?: boolean;
}) {
  recoverInterruptedJobs();
  const existing = getMediaJobByIdempotencyKey(input.idempotencyKey);
  if (existing) {
    if (
      (input.retryFailed && (existing.status === 'failed' || existing.status === 'cancelled')) ||
      (input.retryCompleted && existing.status === 'succeeded')
    ) {
      const updatedAt = nowIso();
      getAppDatabase()
        .prepare(
          `
            UPDATE media_jobs
            SET status = ?, payload_json = ?, error = NULL, updated_at = ?, started_at = NULL, finished_at = NULL
            WHERE id = ?
          `
        )
        .run('queued', serializeJson(input.payload), updatedAt, existing.id);
      return getMediaJobByIdempotencyKey(input.idempotencyKey) ?? existing;
    }

    return existing;
  }

  const id = randomUUID();
  const createdAt = nowIso();
  getAppDatabase()
    .prepare(
      `
        INSERT INTO media_jobs (
          id, type, target_type, target_id, status, idempotency_key, payload_json,
          attempts, max_attempts, error, created_at, updated_at, started_at, finished_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      `
    )
    .run(
      id,
      input.type,
      input.targetType,
      input.targetId,
      'queued',
      input.idempotencyKey,
      serializeJson(input.payload),
      0,
      Math.max(1, Math.floor(input.maxAttempts ?? 3)),
      null,
      createdAt,
      createdAt,
      null,
      null
    );

  return getMediaJobByIdempotencyKey(input.idempotencyKey) as MediaJob;
}

export function createMediaCleanupJob(input: {
  targetType: string;
  targetId: string;
  payload?: unknown;
  idempotencyKey?: string;
}) {
  return upsertMediaJob({
    type: 'file.cleanup',
    targetType: input.targetType,
    targetId: input.targetId,
    idempotencyKey: input.idempotencyKey ?? `file.cleanup:${input.targetType}:${input.targetId}:${randomUUID()}`,
    payload: input.payload,
    maxAttempts: 1
  });
}

export function startMediaJob(jobId: string) {
  return runInTransaction((db) => {
    const row = db.prepare('SELECT * FROM media_jobs WHERE id = ?').get(jobId) as MediaJobRow | undefined;
    if (!row) {
      throw new Error('Media job not found.');
    }

    const job = mediaJobFromRow(row);
    if (job.status === 'succeeded') {
      return job;
    }

    if (job.status !== 'queued' && job.status !== 'failed') {
      throw new Error(`Media job cannot start from ${job.status}.`);
    }

    const updatedAt = nowIso();
    db.prepare(
      `
        UPDATE media_jobs
        SET status = ?, attempts = attempts + 1, error = NULL, updated_at = ?, started_at = ?, finished_at = NULL
        WHERE id = ?
      `
    ).run('running', updatedAt, updatedAt, jobId);

    const updated = db.prepare('SELECT * FROM media_jobs WHERE id = ?').get(jobId) as MediaJobRow;
    return mediaJobFromRow(updated);
  });
}

export function completeMediaJob(jobId: string) {
  const updatedAt = nowIso();
  getAppDatabase()
    .prepare(
      `
        UPDATE media_jobs
        SET status = ?, error = NULL, updated_at = ?, finished_at = ?
        WHERE id = ?
      `
    )
    .run('succeeded', updatedAt, updatedAt, jobId);
}

export function failMediaJob(jobId: string, error: unknown) {
  const updatedAt = nowIso();
  const message = error instanceof Error ? error.message : String(error || 'Media job failed.');
  getAppDatabase()
    .prepare(
      `
        UPDATE media_jobs
        SET status = ?, error = ?, updated_at = ?, finished_at = ?
        WHERE id = ?
      `
    )
    .run('failed', message, updatedAt, updatedAt, jobId);
}

export function isMediaJobPending(idempotencyKey: string) {
  const job = getMediaJobByIdempotencyKey(idempotencyKey);
  return job?.status === 'queued' || job?.status === 'running';
}

export function isMediaJobTargetPending(type: MediaJobType, targetType: string, targetId: string) {
  const job = getMediaJobByTarget(type, targetType, targetId);
  return job?.status === 'queued' || job?.status === 'running';
}

export function recordMediaFileAction(input: {
  jobId: string;
  actionType: string;
  status: 'planned' | 'running' | 'succeeded' | 'failed';
  filePath: string;
  backupPath?: string | null;
  metadata?: unknown;
}) {
  const now = nowIso();
  getAppDatabase()
    .prepare(
      `
        INSERT INTO media_file_actions (
          id, job_id, action_type, status, file_path, backup_path, metadata_json, created_at, updated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      `
    )
    .run(
      randomUUID(),
      input.jobId,
      input.actionType,
      input.status,
      input.filePath,
      input.backupPath ?? null,
      serializeJson(input.metadata),
      now,
      now
    );
}

export function listMediaFileActionsForJob(jobId: string) {
  const rows = getAppDatabase()
    .prepare(
      `
        SELECT * FROM media_file_actions
        WHERE job_id = ?
        ORDER BY created_at ASC
      `
    )
    .all(jobId) as MediaFileActionRow[];
  return rows.map(mediaFileActionFromRow);
}

export function completedMediaJob(input: {
  type: MediaJobType;
  targetType: string;
  targetId: string;
  idempotencyKey: string;
  payload?: unknown;
}) {
  const job = upsertMediaJob({ ...input, maxAttempts: 1 });
  if (job.status !== 'succeeded') {
    startMediaJob(job.id);
    completeMediaJob(job.id);
  }
  return getMediaJobByIdempotencyKey(input.idempotencyKey) ?? job;
}

export async function writeStreamAndHash(input: {
  stream: ReadableStream<Uint8Array>;
  absolutePath: string;
}): Promise<MediaFingerprint> {
  await fsp.mkdir(path.dirname(input.absolutePath), { recursive: true });

  const hash = createHash('sha256');
  let contentSizeBytes = 0;
  const hashingStream = new Transform({
    transform(chunk, _encoding, callback) {
      const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
      hash.update(buffer);
      contentSizeBytes += buffer.length;
      callback(null, buffer);
    }
  });

  await pipeline(
    Readable.fromWeb(input.stream as unknown as Parameters<typeof Readable.fromWeb>[0]),
    hashingStream,
    fs.createWriteStream(input.absolutePath)
  );

  return {
    contentHash: `sha256:${hash.digest('hex')}`,
    contentHashAlgorithm: 'sha256',
    contentSizeBytes
  };
}

export async function writeBufferAndHash(input: {
  buffer: Buffer;
  absolutePath: string;
}): Promise<MediaFingerprint> {
  await fsp.mkdir(path.dirname(input.absolutePath), { recursive: true });
  const hash = createHash('sha256');
  hash.update(input.buffer);
  await fsp.writeFile(input.absolutePath, input.buffer);
  return {
    contentHash: `sha256:${hash.digest('hex')}`,
    contentHashAlgorithm: 'sha256',
    contentSizeBytes: input.buffer.length
  };
}

export async function hashFile(absolutePath: string): Promise<MediaFingerprint> {
  const hash = createHash('sha256');
  let contentSizeBytes = 0;
  await pipeline(
    fs.createReadStream(absolutePath),
    new Transform({
      transform(chunk, _encoding, callback) {
        const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
        hash.update(buffer);
        contentSizeBytes += buffer.length;
        callback();
      }
    })
  );

  return {
    contentHash: `sha256:${hash.digest('hex')}`,
    contentHashAlgorithm: 'sha256',
    contentSizeBytes
  };
}

function normalizeTrashPathPart(filePath: string) {
  const normalized = path.posix.normalize(filePath.replaceAll(path.sep, '/')).replace(/^\/+/, '');
  if (!normalized || normalized === '.' || normalized.startsWith('../') || normalized.includes('/../')) {
    throw new Error('Invalid trash file path.');
  }
  return normalized;
}

function mediaTrashRoot() {
  return path.join(resolveDataDir(), 'media-trash');
}

function absoluteTrashPath(backupPath: string) {
  const relativeBackupPath = normalizeTrashPathPart(backupPath);
  const absolutePath = path.resolve(resolveDataDir(), relativeBackupPath);
  const root = path.resolve(mediaTrashRoot());
  const normalizedRoot = root.endsWith(path.sep) ? root : `${root}${path.sep}`;
  if (absolutePath !== root && !absolutePath.startsWith(normalizedRoot)) {
    throw new Error('Invalid media trash path.');
  }
  return absolutePath;
}

async function pathExists(absolutePath: string) {
  try {
    await fsp.access(absolutePath);
    return true;
  } catch {
    return false;
  }
}

function managedVideoFileEntries(filePath: string) {
  const normalizedFilePath = normalizeManagedVideoPath(filePath);
  const posterExtensions = ['.jpg', '.jpeg', '.webp', '.png', '.avif'];
  return [
    {
      filePath: normalizedFilePath,
      absolutePath: resolveManagedVideoAbsolutePath(normalizedFilePath),
      kind: 'video'
    },
    ...posterExtensions.map((extension) => {
      const posterPath = managedPosterPathForVideo(normalizedFilePath, extension);
      return {
        filePath: path.posix.join('video-posters', posterPath),
        absolutePath: resolvePathInsideRoot(resolvePosterRoot(), posterPath),
        kind: 'poster'
      };
    })
  ];
}

export async function moveFileToMediaTrash(input: {
  jobId: string;
  filePath: string;
  absolutePath: string;
  actionType?: string;
  metadata?: Record<string, unknown>;
}) {
  const logicalPath = normalizeTrashPathPart(input.filePath);
  const backupPath = path.posix.join('media-trash', input.jobId, logicalPath);
  const absoluteBackupPath = absoluteTrashPath(backupPath);
  const metadata = {
    ...(input.metadata ?? {}),
    originalAbsolutePath: input.absolutePath
  };

  if (!(await pathExists(input.absolutePath))) {
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType ?? 'move-to-trash',
      status: 'succeeded',
      filePath: logicalPath,
      backupPath: null,
      metadata: { ...metadata, missing: true }
    });
    return { filePath: logicalPath, backupPath: null, moved: false };
  }

  await fsp.mkdir(path.dirname(absoluteBackupPath), { recursive: true });
  await fsp.rename(input.absolutePath, absoluteBackupPath);
  recordMediaFileAction({
    jobId: input.jobId,
    actionType: input.actionType ?? 'move-to-trash',
    status: 'succeeded',
    filePath: logicalPath,
    backupPath,
    metadata
  });

  return { filePath: logicalPath, backupPath, moved: true };
}

export async function trashManagedVideoFiles(input: {
  jobId: string;
  filePath: string;
  actionType?: string;
  metadata?: Record<string, unknown>;
}) {
  const results = [];
  for (const entry of managedVideoFileEntries(input.filePath)) {
    results.push(
      await moveFileToMediaTrash({
        jobId: input.jobId,
        actionType: input.actionType ?? 'move-to-trash',
        filePath: entry.filePath,
        absolutePath: entry.absolutePath,
        metadata: {
          ...(input.metadata ?? {}),
          managedVideoPath: normalizeManagedVideoPath(input.filePath),
          fileKind: entry.kind
        }
      })
    );
  }
  return results;
}

export async function trashManagedVideoFileSet(input: {
  jobId: string;
  filePaths: string[];
  actionType?: string;
  metadata?: Record<string, unknown>;
}) {
  const seen = new Set<string>();
  const results = [];
  for (const filePath of input.filePaths) {
    const normalizedFilePath = normalizeManagedVideoPath(filePath);
    if (seen.has(normalizedFilePath)) {
      continue;
    }
    seen.add(normalizedFilePath);
    results.push(
      ...(await trashManagedVideoFiles({
        jobId: input.jobId,
        filePath: normalizedFilePath,
        actionType: input.actionType,
        metadata: input.metadata
      }))
    );
  }
  return results;
}

async function renameFileWithAction(input: {
  jobId: string;
  actionType: string;
  fromPath: string;
  toPath: string;
  fromAbsolutePath: string;
  toAbsolutePath: string;
  metadata?: Record<string, unknown>;
}) {
  if (input.fromAbsolutePath === input.toAbsolutePath) {
    return { fromPath: input.fromPath, toPath: input.toPath, renamed: false };
  }

  const metadata = {
    ...(input.metadata ?? {}),
    originalAbsolutePath: input.fromAbsolutePath,
    destinationAbsolutePath: input.toAbsolutePath
  };

  if (!(await pathExists(input.fromAbsolutePath))) {
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType,
      status: 'succeeded',
      filePath: normalizeTrashPathPart(input.fromPath),
      backupPath: normalizeTrashPathPart(input.toPath),
      metadata: { ...metadata, missing: true }
    });
    return { fromPath: input.fromPath, toPath: input.toPath, renamed: false };
  }

  try {
    await fsp.mkdir(path.dirname(input.toAbsolutePath), { recursive: true });
    await fsp.rename(input.fromAbsolutePath, input.toAbsolutePath);
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType,
      status: 'succeeded',
      filePath: normalizeTrashPathPart(input.fromPath),
      backupPath: normalizeTrashPathPart(input.toPath),
      metadata
    });
    return { fromPath: input.fromPath, toPath: input.toPath, renamed: true };
  } catch (error) {
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType,
      status: 'failed',
      filePath: normalizeTrashPathPart(input.fromPath),
      backupPath: normalizeTrashPathPart(input.toPath),
      metadata: {
        ...metadata,
        error: error instanceof Error ? error.message : String(error)
      }
    });
    throw error;
  }
}

export async function renameManagedVideoFiles(input: {
  jobId: string;
  fromPath: string;
  toPath: string;
  metadata?: Record<string, unknown>;
}) {
  const normalizedFromPath = normalizeManagedVideoPath(input.fromPath);
  const normalizedToPath = normalizeManagedVideoPath(input.toPath);
  if (normalizedFromPath === normalizedToPath) {
    return [];
  }

  const fromEntries = managedVideoFileEntries(normalizedFromPath);
  const toEntries = managedVideoFileEntries(normalizedToPath);
  const results = [];
  for (let index = 0; index < fromEntries.length; index += 1) {
    const fromEntry = fromEntries[index];
    const toEntry = toEntries[index];
    results.push(
      await renameFileWithAction({
        jobId: input.jobId,
        actionType: fromEntry.kind === 'video' ? 'rename-video' : 'rename-poster',
        fromPath: fromEntry.filePath,
        toPath: toEntry.filePath,
        fromAbsolutePath: fromEntry.absolutePath,
        toAbsolutePath: toEntry.absolutePath,
        metadata: {
          ...(input.metadata ?? {}),
          managedVideoPath: normalizedFromPath,
          nextManagedVideoPath: normalizedToPath,
          fileKind: fromEntry.kind
        }
      })
    );
  }
  return results;
}

export async function deleteTemporaryFile(input: {
  jobId: string;
  absolutePath: string;
  filePath: string;
  actionType?: string;
  metadata?: Record<string, unknown>;
}) {
  const logicalPath = normalizeTrashPathPart(input.filePath);
  const metadata = {
    ...(input.metadata ?? {}),
    originalAbsolutePath: input.absolutePath
  };

  if (!(await pathExists(input.absolutePath))) {
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType ?? 'delete-temp',
      status: 'succeeded',
      filePath: logicalPath,
      metadata: { ...metadata, missing: true }
    });
    return { filePath: logicalPath, deleted: false };
  }

  try {
    await fsp.unlink(input.absolutePath);
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType ?? 'delete-temp',
      status: 'succeeded',
      filePath: logicalPath,
      metadata
    });
    return { filePath: logicalPath, deleted: true };
  } catch (error) {
    recordMediaFileAction({
      jobId: input.jobId,
      actionType: input.actionType ?? 'delete-temp',
      status: 'failed',
      filePath: logicalPath,
      metadata: {
        ...metadata,
        error: error instanceof Error ? error.message : String(error)
      }
    });
    throw error;
  }
}

export async function restoreTrashedFilesForJob(jobId: string) {
  const actions = listMediaFileActionsForJob(jobId)
    .filter((action) => action.status === 'succeeded' && action.backupPath)
    .reverse();
  const restoreEntries: Array<{
    filePath: string;
    backupAbsolutePath: string;
    destination: string;
  }> = [];

  for (const action of actions) {
    const metadata = action.metadata && typeof action.metadata === 'object' ? (action.metadata as Record<string, unknown>) : {};
    const destination = typeof metadata.originalAbsolutePath === 'string' ? metadata.originalAbsolutePath : null;
    if (!destination || !action.backupPath) {
      continue;
    }

    const backupAbsolutePath = absoluteTrashPath(action.backupPath);
    const destinationExists = await pathExists(destination);
    const backupExists = await pathExists(backupAbsolutePath);
    if (destinationExists) {
      continue;
    }
    if (!backupExists) {
      throw new Error(`Missing trashed media file for ${action.filePath}.`);
    }
    restoreEntries.push({
      filePath: action.filePath,
      backupAbsolutePath,
      destination
    });
  }

  const restored: string[] = [];
  for (const entry of restoreEntries) {
    await fsp.mkdir(path.dirname(entry.destination), { recursive: true });
    await fsp.rename(entry.backupAbsolutePath, entry.destination);
    restored.push(entry.filePath);
  }

  return restored;
}
