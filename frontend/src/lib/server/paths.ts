import path from 'node:path';

export const LEGACY_MOVE_VIDEO_PREFIX = 'videomoves';
export const MOVE_VIDEO_PREFIX = 'video-moves';
export const SOURCE_VIDEO_PREFIX = 'video-sources';

export function resolveDataDir() {
  return path.resolve(process.cwd(), process.env.DATA_DIR ?? '../migration-data');
}

export function resolveMediaRoot() {
  return path.resolve(process.cwd(), process.env.MEDIA_ROOT ?? '../video-moves');
}

export function resolveSourceRoot() {
  return path.resolve(process.cwd(), process.env.SOURCE_ROOT ?? '../video-sources');
}

export function resolvePosterRoot() {
  return path.resolve(process.cwd(), process.env.POSTER_ROOT ?? '../video-posters');
}

export function resolvePathInsideRoot(root: string, relativePath: string) {
  const absolutePath = path.resolve(root, relativePath);
  const normalizedRoot = root.endsWith(path.sep) ? root : `${root}${path.sep}`;

  if (absolutePath !== root && !absolutePath.startsWith(normalizedRoot)) {
    throw new Error('Invalid relative path');
  }

  return absolutePath;
}

export function normalizeManagedVideoPath(filePath: string) {
  return filePath.replace(new RegExp(`^${LEGACY_MOVE_VIDEO_PREFIX}/`), `${MOVE_VIDEO_PREFIX}/`);
}

export function isMoveVideoPath(filePath: string) {
  const normalized = normalizeManagedVideoPath(filePath);
  return normalized === MOVE_VIDEO_PREFIX || normalized.startsWith(`${MOVE_VIDEO_PREFIX}/`);
}

export function isSourceVideoPath(filePath: string) {
  const normalized = normalizeManagedVideoPath(filePath);
  return normalized === SOURCE_VIDEO_PREFIX || normalized.startsWith(`${SOURCE_VIDEO_PREFIX}/`);
}

export function resolveManagedVideoAbsolutePath(filePath: string) {
  const normalized = normalizeManagedVideoPath(filePath);

  if (isMoveVideoPath(normalized)) {
    const relativePath = normalized.replace(new RegExp(`^${MOVE_VIDEO_PREFIX}/?`), '');
    return resolvePathInsideRoot(resolveMediaRoot(), relativePath);
  }

  if (isSourceVideoPath(normalized)) {
    const relativePath = normalized.replace(new RegExp(`^${SOURCE_VIDEO_PREFIX}/?`), '');
    return resolvePathInsideRoot(resolveSourceRoot(), relativePath);
  }

  throw new Error('Unsupported managed video path');
}

export function managedPosterPathForVideo(filePath: string, extension: string) {
  const normalized = normalizeManagedVideoPath(filePath);
  const parsed = path.parse(normalized);
  return path.posix.join(parsed.dir, `${parsed.name}${extension}`);
}
