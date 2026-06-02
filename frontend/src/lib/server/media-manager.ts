import fs from 'node:fs';
import fsp from 'node:fs/promises';
import path from 'node:path';
import { randomUUID, createHash } from 'node:crypto';
import { Transform, Readable } from 'node:stream';
import { pipeline } from 'node:stream/promises';
import { getAppDatabase, runInTransaction } from './app-state';

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
