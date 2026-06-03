import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

export async function setupMediaTestEnvironment(testName) {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), `salsa-${testName}-`));
  const dataDir = path.join(tempRoot, 'data');
  const appStateBootstrapDir = path.join(dataDir, 'bootstrap', 'app-state');
  const catalogBootstrapDir = path.join(dataDir, 'bootstrap', 'catalog');
  const mediaRoot = path.join(tempRoot, 'video-moves');
  const sourceRoot = path.join(tempRoot, 'video-sources');
  const posterRoot = path.join(tempRoot, 'video-posters');

  process.env.DATA_DIR = dataDir;
  process.env.MEDIA_ROOT = mediaRoot;
  process.env.SOURCE_ROOT = sourceRoot;
  process.env.POSTER_ROOT = posterRoot;

  await Promise.all([
    fs.mkdir(appStateBootstrapDir, { recursive: true }),
    fs.mkdir(catalogBootstrapDir, { recursive: true }),
    fs.mkdir(mediaRoot),
    fs.mkdir(sourceRoot),
    fs.mkdir(posterRoot)
  ]);

  return {
    tempRoot,
    dataDir,
    appStateBootstrapDir,
    catalogBootstrapDir,
    mediaRoot,
    sourceRoot,
    posterRoot,
    libraryPath: path.join(appStateBootstrapDir, 'video-library.json')
  };
}

export async function cleanupMediaTestEnvironment(tempRoot) {
  await fs.rm(tempRoot, { recursive: true, force: true });
}

export async function exists(filePath) {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

export async function writeFile(filePath, contents = 'media') {
  await fs.mkdir(path.dirname(filePath), { recursive: true });
  await fs.writeFile(filePath, contents);
}

export async function writeLibrary(libraryPath, library) {
  await fs.writeFile(libraryPath, `${JSON.stringify(library, null, 2)}\n`);
  const { writeMediaCatalog } = await import('../src/lib/server/media-catalog.ts');
  await writeMediaCatalog(library);
}

export async function readLibrary(libraryPath) {
  const { readMediaCatalog } = await import('../src/lib/server/media-catalog.ts');
  return readMediaCatalog();
}

export function sourceAsset(overrides = {}) {
  return {
    id: 'source-asset',
    kind: 'source',
    filePath: 'video-sources/source.mp4',
    displayName: 'Source Clip',
    originalFilename: 'source.mp4',
    dancers: ['A'],
    timing: 'on1',
    contentType: 'music',
    environment: 'class',
    originType: 'self-recorded',
    recordDate: null,
    classWorkshop: null,
    tags: [],
    notes: null,
    contentHash: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    contentHashAlgorithm: 'sha256',
    contentSizeBytes: 6,
    hashStatus: 'ready',
    createdAt: '2026-06-02T00:00:00.000Z',
    ...overrides
  };
}

export function moveAsset(overrides = {}) {
  return {
    id: 'output-asset',
    kind: 'move',
    filePath: 'video-moves/RT000001 Source Clip abcdef12.mp4',
    displayName: 'Source Clip',
    originalFilename: 'RT000001 Source Clip abcdef12.mp4',
    dancers: ['A'],
    timing: 'on1',
    contentType: 'music',
    environment: 'class',
    originType: 'self-recorded',
    recordDate: null,
    classWorkshop: null,
    tags: [],
    notes: null,
    contentHash: null,
    contentHashAlgorithm: null,
    contentSizeBytes: null,
    hashStatus: 'pending',
    createdAt: '2026-06-02T00:00:01.000Z',
    ...overrides
  };
}

export function derivedClip(overrides = {}) {
  return {
    id: 'abcdef12-0000-4000-9000-000000000000',
    sourceAssetId: 'source-asset',
    moveId: 'RT000001',
    moveDisplayId: 'RT000001',
    isKeyVideo: true,
    label: 'Source Clip',
    descriptorLabel: null,
    startPositionId: null,
    endPositionId: null,
    timingGroupId: null,
    manuallyNamed: false,
    startMs: 0,
    endMs: 3000,
    actionStartMs: 500,
    actionEndMs: 2500,
    cropRect: null,
    countMarkers: [],
    countOverlayPlacement: 'top-left',
    countTimingPreset: 'on2-default',
    outputAssetId: 'output-asset',
    actionOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 action.mp4',
    lowResOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 low.mp4',
    lowResPaddedOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 padded low.mp4',
    publishedAssetId: 'output-asset',
    publishedActionOutputFilePath: 'video-moves/RT000001 Source Clip abcdef12 action.mp4',
    publishedLowResFilePath: 'video-moves/RT000001 Source Clip abcdef12 low.mp4',
    publishedLowResPaddedFilePath: 'video-moves/RT000001 Source Clip abcdef12 padded low.mp4',
    publishedAt: '2026-06-02T00:00:03.000Z',
    status: 'ready',
    error: null,
    createdAt: '2026-06-02T00:00:03.000Z',
    updatedAt: '2026-06-02T00:00:03.000Z',
    ...overrides
  };
}
