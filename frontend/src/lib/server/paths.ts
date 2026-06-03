import path from 'node:path';
import fs from 'node:fs';

export const LEGACY_MOVE_VIDEO_PREFIX = 'videomoves';
export const MOVE_VIDEO_PREFIX = 'video-moves';
export const SOURCE_VIDEO_PREFIX = 'video-sources';

function resolveMaybeUpgradedPath(
  configuredPath: string | undefined,
  defaultPath: string,
  upgradedFromLegacyParent: string,
  legacyBasename: string
) {
  const configured = configuredPath ? path.resolve(process.cwd(), configuredPath) : path.resolve(process.cwd(), defaultPath);
  if (configuredPath && path.basename(configured) === legacyBasename) {
    const upgraded = path.join(path.dirname(configured), upgradedFromLegacyParent);
    if (fs.existsSync(upgraded)) {
      return upgraded;
    }
  }
  return configured;
}

export function resolveDataDir() {
  return resolveMaybeUpgradedPath(process.env.DATA_DIR, '../data/live', path.join('data', 'live'), 'migration-data');
}

export function resolveCatalogBootstrapDir() {
  return path.join(resolveDataDir(), 'bootstrap', 'catalog');
}

export function resolveAppStateBootstrapDir() {
  return path.join(resolveDataDir(), 'bootstrap', 'app-state');
}

export function resolveMediaRoot() {
  return resolveMaybeUpgradedPath(
    process.env.MEDIA_ROOT,
    '../data/live/media/video-moves',
    path.join('data', 'live', 'media', 'video-moves'),
    'video-moves'
  );
}

export function resolveSourceRoot() {
  return resolveMaybeUpgradedPath(
    process.env.SOURCE_ROOT,
    '../data/live/media/video-sources',
    path.join('data', 'live', 'media', 'video-sources'),
    'video-sources'
  );
}

export function resolvePosterRoot() {
  return resolveMaybeUpgradedPath(
    process.env.POSTER_ROOT,
    '../data/live/media/video-posters',
    path.join('data', 'live', 'media', 'video-posters'),
    'video-posters'
  );
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
