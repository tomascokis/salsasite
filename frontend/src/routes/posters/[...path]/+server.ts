import fs from 'node:fs/promises';
import { createReadStream } from 'node:fs';
import path from 'node:path';
import { Readable } from 'node:stream';
import { error } from '@sveltejs/kit';
import { resolvePathInsideRoot, resolvePosterRoot } from '$lib/server/paths';

function mimeTypeFor(filename: string) {
  const ext = path.extname(filename).toLowerCase();
  if (ext === '.png') return 'image/png';
  if (ext === '.webp') return 'image/webp';
  if (ext === '.avif') return 'image/avif';
  return 'image/jpeg';
}

export async function GET({ params }) {
  const rawPath = params.path ?? '';
  const decodedPath = decodeURIComponent(rawPath);
  const posterRoot = resolvePosterRoot();
  let absolutePath: string;
  try {
    absolutePath = resolvePathInsideRoot(posterRoot, decodedPath);
  } catch {
    throw error(400, 'Invalid poster path');
  }

  let stat;
  try {
    stat = await fs.stat(absolutePath);
  } catch {
    throw error(404, 'Poster file not found');
  }

  const stream = createReadStream(absolutePath);
  return new Response(Readable.toWeb(stream) as ReadableStream, {
    headers: {
      'content-length': String(stat.size),
      'content-type': mimeTypeFor(absolutePath),
      'cache-control': 'public, max-age=3600'
    }
  });
}
