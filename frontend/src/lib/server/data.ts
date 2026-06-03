import type {
  LayoutColumn,
  MoveRecord,
  ProgressSnapshot,
  RawMoveReferenceRecord,
  SiteManifest
} from '$lib/types';
import { buildResolvedMoveVideoMetadataIndex } from './video-library';
import { resolveDataDir, resolveMediaRoot, resolvePosterRoot, resolveSourceRoot } from './paths';
import { applyMoveEditStore } from './move-editor';
import { buildOverviewLayout, buildOverviewSearchIndex } from './overview.js';
import { getAppDatabase } from './app-state';

type CatalogCache = {
  manifest?: SiteManifest;
  moves?: MoveRecord[];
  layout?: LayoutColumn[];
  progress?: ProgressSnapshot[];
  rawMoves?: RawMoveReferenceRecord[];
};

type DatabaseRow = Record<string, unknown>;

const cache: CatalogCache = {};
export { resolveDataDir, resolveMediaRoot, resolvePosterRoot, resolveSourceRoot } from './paths';

function parseJson<T>(value: unknown): T {
  return JSON.parse(String(value)) as T;
}

function rows<T>(sql: string): T[] {
  return getAppDatabase().prepare(sql).all() as T[];
}

function defaultManifest(): SiteManifest {
  return {
    generatedAt: new Date(0).toISOString(),
    source: {
      rawMoveReference: 'data/legacy/reference/data_reference.xlsx',
      moves: 'data/live/bootstrap/catalog/moves.json',
      layout: 'data/live/bootstrap/catalog/layout.json',
      progress: 'data/live/bootstrap/catalog/progress.json',
      localVideoDirectory: null,
      visualReference: ''
    },
    counts: {
      moveRows: 0,
      validMoves: 0,
      layoutRows: 0,
      layoutColumns: 0,
      progressRows: 0,
      progressSnapshots: 0,
      trackableMoves: 0,
      movesWithLocalVideo: 0
    },
    routes: {
      home: '/',
      moveDetail: '/moves/[slug]',
      progress: '/progress',
      progressEditor: '/progress/editor'
    }
  };
}

function readManifestFromCatalog(): SiteManifest {
  const row = getAppDatabase().prepare('SELECT manifest_json FROM catalog_manifest WHERE id = ?').get('main') as
    | { manifest_json?: string }
    | undefined;
  return row?.manifest_json ? parseJson<SiteManifest>(row.manifest_json) : defaultManifest();
}

function readMovesFromCatalog(): MoveRecord[] {
  return rows<DatabaseRow>('SELECT move_json FROM catalog_moves ORDER BY sort_order').map((row) =>
    parseJson<MoveRecord>(row.move_json)
  );
}

function readLayoutFromCatalog(): LayoutColumn[] {
  return rows<DatabaseRow>('SELECT column_json FROM catalog_layout_columns ORDER BY sort_order').map((row) =>
    parseJson<LayoutColumn>(row.column_json)
  );
}

function readProgressFromCatalog(): ProgressSnapshot[] {
  return rows<DatabaseRow>('SELECT snapshot_json FROM catalog_progress_snapshots ORDER BY sort_order').map((row) =>
    parseJson<ProgressSnapshot>(row.snapshot_json)
  );
}

function readRawMovesFromCatalog(): RawMoveReferenceRecord[] {
  return rows<DatabaseRow>('SELECT reference_json FROM catalog_raw_move_references ORDER BY sort_order').map((row) =>
    parseJson<RawMoveReferenceRecord>(row.reference_json)
  );
}

export async function getManifest() {
  cache.manifest ??= readManifestFromCatalog();
  const moves = await getMoves();

  return {
    ...cache.manifest,
    counts: {
      ...cache.manifest.counts,
      moveRows: moves.length,
      movesWithLocalVideo: moves.filter((move) => move.hasLocalVideo).length
    }
  };
}

export async function getMoves() {
  cache.moves ??= readMovesFromCatalog();
  const editedMoves = await applyMoveEditStore(cache.moves);
  const resolvedVideoIndex = await buildResolvedMoveVideoMetadataIndex(editedMoves);

  return editedMoves.map((move) => {
    const videoMetadata = resolvedVideoIndex.get(move.id.toUpperCase());
    const localVideoFiles = videoMetadata?.files ?? [];

    return {
      ...move,
      hasLocalVideo: localVideoFiles.length > 0,
      videoFiles: localVideoFiles,
      previewVideoFile: videoMetadata?.previewFile ?? null
    };
  });
}

export async function getLayout() {
  cache.layout ??= readLayoutFromCatalog();
  return cache.layout;
}

export async function getOverviewLayout() {
  const [layout, moves] = await Promise.all([getLayout(), getMoves()]);
  return buildOverviewLayout(layout, moves);
}

export async function getProgressSnapshots() {
  cache.progress ??= readProgressFromCatalog();
  return cache.progress;
}

export async function getSearchIndex() {
  const moves = await getMoves();
  return buildOverviewSearchIndex(moves);
}

export async function getRawMoveReference() {
  cache.rawMoves ??= readRawMovesFromCatalog();
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
