import fs from 'node:fs/promises';
import { createReadStream } from 'node:fs';
import path from 'node:path';
import { Readable } from 'node:stream';
import { error } from '@sveltejs/kit';
import { resolveManagedVideoAbsolutePath } from '$lib/server/paths';
import type { RequestHandler } from './$types';

function mimeTypeFor(filename: string) {
  const ext = path.extname(filename).toLowerCase();
  if (ext === '.mov') return 'video/quicktime';
  if (ext === '.m4v') return 'video/mp4';
  return 'video/mp4';
}

export const GET: RequestHandler = async ({ params, request }) => {
  const rawPath = params.path ?? '';
  const decodedPath = decodeURIComponent(rawPath);
  let absolutePath: string;
  try {
    absolutePath = resolveManagedVideoAbsolutePath(decodedPath);
  } catch {
    throw error(400, 'Invalid media path');
  }

  let stat;
  try {
    stat = await fs.stat(absolutePath);
  } catch {
    throw error(404, 'Media file not found');
  }

  const range = request.headers.get('range');
  if (!range) {
    const stream = createReadStream(absolutePath);
    return new Response(Readable.toWeb(stream) as ReadableStream, {
      headers: {
        'content-length': String(stat.size),
        'content-type': mimeTypeFor(absolutePath),
        'accept-ranges': 'bytes',
        'cache-control': 'public, max-age=3600'
      }
    });
  }

  const match = /bytes=(\d*)-(\d*)/.exec(range);
  if (!match) {
    throw error(416, 'Invalid range');
  }

  const start = match[1] ? Number(match[1]) : 0;
  const end = match[2] ? Number(match[2]) : stat.size - 1;

  if (Number.isNaN(start) || Number.isNaN(end) || start > end || end >= stat.size) {
    throw error(416, 'Range not satisfiable');
  }

  const stream = createReadStream(absolutePath, { start, end });
  return new Response(Readable.toWeb(stream) as ReadableStream, {
    status: 206,
    headers: {
      'content-length': String(end - start + 1),
      'content-range': `bytes ${start}-${end}/${stat.size}`,
      'content-type': mimeTypeFor(absolutePath),
      'accept-ranges': 'bytes',
      'cache-control': 'public, max-age=3600'
    }
  });
};
