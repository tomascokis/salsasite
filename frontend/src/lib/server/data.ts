import fs from 'node:fs/promises';
import type {
  LayoutColumn,
  MoveRecord,
  ProgressSnapshot,
  RawMoveReferenceRecord,
  SearchIndexEntry,
  SiteManifest
} from '$lib/types';
import { buildResolvedMoveVideoIndex } from './video-library';
import { resolveDataDir, resolveMediaRoot, resolvePosterRoot, resolveSourceRoot } from './paths';
import { applyMoveEditStore } from './move-editor';

type JsonCache = {
  manifest?: SiteManifest;
  moves?: MoveRecord[];
  layout?: LayoutColumn[];
  progress?: ProgressSnapshot[];
  search?: SearchIndexEntry[];
  rawMoves?: RawMoveReferenceRecord[];
};

const cache: JsonCache = {};
export { resolveDataDir, resolveMediaRoot, resolvePosterRoot, resolveSourceRoot } from './paths';

async function readJsonFile<T>(filename: string): Promise<T> {
  const fullPath = `${resolveDataDir()}/${filename}`;
  const contents = await fs.readFile(fullPath, 'utf-8');
  return JSON.parse(contents) as T;
}

export async function getManifest() {
  cache.manifest ??= await readJsonFile<SiteManifest>('manifest.json');
  const moves = await getMoves();

  return {
    ...cache.manifest,
    counts: {
      ...cache.manifest.counts,
      movesWithLocalVideo: moves.filter((move) => move.hasLocalVideo).length
    }
  };
}

export async function getMoves() {
  cache.moves ??= await readJsonFile<MoveRecord[]>('moves.json');
  const editedMoves = await applyMoveEditStore(cache.moves);
  const resolvedVideoIndex = await buildResolvedMoveVideoIndex(editedMoves);

  return editedMoves.map((move) => {
    const localVideoFiles = resolvedVideoIndex.get(move.id.toUpperCase()) ?? [];

    return {
      ...move,
      hasLocalVideo: localVideoFiles.length > 0,
      videoFiles: localVideoFiles
    };
  });
}

export async function getLayout() {
  cache.layout ??= await readJsonFile<LayoutColumn[]>('layout.json');
  return cache.layout;
}

export async function getProgressSnapshots() {
  cache.progress ??= await readJsonFile<ProgressSnapshot[]>('progress.json');
  return cache.progress;
}

export async function getSearchIndex() {
  cache.search ??= await readJsonFile<SearchIndexEntry[]>('search-index.json');
  return cache.search;
}

export async function getRawMoveReference() {
  cache.rawMoves ??= await readJsonFile<RawMoveReferenceRecord[]>('raw-moves.json');
  return cache.rawMoves;
}

export async function getMoveBySlug(slug: string) {
  const moves = await getMoves();
  return moves.find((move) => move.slug === slug) ?? null;
}

export async function getLatestSnapshot() {
  const snapshots = await getProgressSnapshots();
  return snapshots[0] ?? null;
}
