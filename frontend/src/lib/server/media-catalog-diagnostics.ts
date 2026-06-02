import fs from 'node:fs/promises';
import path from 'node:path';
import type { DerivedClip, VideoAsset } from '$lib/types';
import { getAppDatabase } from './app-state';
import { readMediaCatalog } from './media-catalog';
import {
  normalizeManagedVideoPath,
  resolveDataDir,
  resolveManagedVideoAbsolutePath
} from './paths';

const SQLITE_BOOTSTRAP_META_KEY = 'media_catalog_sqlite_v1';
const EXPORT_DIRNAME = 'media-catalog-exports';

export type MediaCatalogDiagnosticFinding = {
  severity: 'warning' | 'error';
  type: string;
  entityType: string;
  entityId: string;
  message: string;
  filePath?: string;
  relatedId?: string;
};

export type MediaCatalogDiagnostics = {
  checkedAt: string;
  database: {
    pathLabel: 'DATA_DIR/app-state.sqlite';
    bootstrapStatus: 'complete' | 'incomplete';
    bootstrapMetaValue: string | null;
  };
  counts: {
    videoAssets: number;
    sourceAssets: number;
    moveAssets: number;
    moveVideoLinks: number;
    derivedClips: number;
    clipsByStatus: Record<DerivedClip['status'], number>;
  };
  latestExport: {
    filePath: string;
    exportedAt: string | null;
  } | null;
  integritySummary: {
    total: number;
    errors: number;
    warnings: number;
    byType: Record<string, number>;
  };
  findings: MediaCatalogDiagnosticFinding[];
};

function exportDirectory() {
  return path.join(resolveDataDir(), EXPORT_DIRNAME);
}

function parseExportTimestamp(filename: string) {
  const match = /^video-library-(\d{4}-\d{2}-\d{2})T(\d{2})-(\d{2})-(\d{2})-(\d{3})Z\.json$/.exec(filename);
  if (!match) {
    return null;
  }

  const [, date, hour, minute, second, millisecond] = match;
  const iso = `${date}T${hour}:${minute}:${second}.${millisecond}Z`;
  const parsed = new Date(iso);
  return Number.isNaN(parsed.getTime()) ? null : parsed.toISOString();
}

async function latestCatalogExport() {
  try {
    const entries = await fs.readdir(exportDirectory(), { withFileTypes: true });
    const exports = await Promise.all(
      entries
        .filter((entry) => entry.isFile() && /^video-library-.*\.json$/.test(entry.name))
        .map(async (entry) => {
          const absolutePath = path.join(exportDirectory(), entry.name);
          const stat = await fs.stat(absolutePath);
          return {
            filePath: `${EXPORT_DIRNAME}/${entry.name}`,
            exportedAt: parseExportTimestamp(entry.name),
            mtimeMs: stat.mtimeMs
          };
        })
    );

    exports.sort((left, right) => right.mtimeMs - left.mtimeMs);
    const latest = exports[0];
    return latest ? { filePath: latest.filePath, exportedAt: latest.exportedAt } : null;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return null;
    }
    throw error;
  }
}

function bootstrapMetaValue() {
  const row = getAppDatabase().prepare('SELECT value FROM app_state_meta WHERE key = ?').get(SQLITE_BOOTSTRAP_META_KEY) as
    | { value: string }
    | undefined;
  return row?.value ?? null;
}

async function pathExists(filePath: string) {
  try {
    await fs.access(resolveManagedVideoAbsolutePath(filePath));
    return true;
  } catch {
    return false;
  }
}

function addFinding(findings: MediaCatalogDiagnosticFinding[], finding: MediaCatalogDiagnosticFinding) {
  findings.push(finding);
}

async function checkAssetFile(findings: MediaCatalogDiagnosticFinding[], asset: VideoAsset) {
  const filePath = normalizeManagedVideoPath(asset.filePath);
  try {
    resolveManagedVideoAbsolutePath(filePath);
  } catch {
    addFinding(findings, {
      severity: 'error',
      type: 'invalid-managed-file-path',
      entityType: 'media:asset',
      entityId: asset.id,
      filePath,
      message: 'Video asset path is not under a managed media root.'
    });
    return;
  }

  if (!(await pathExists(filePath))) {
    addFinding(findings, {
      severity: 'warning',
      type: 'missing-asset-file',
      entityType: 'media:asset',
      entityId: asset.id,
      filePath,
      message: 'Video asset file is missing from disk.'
    });
  }
}

async function checkClipVariantPath(
  findings: MediaCatalogDiagnosticFinding[],
  clip: DerivedClip,
  key: keyof Pick<
    DerivedClip,
    | 'actionOutputFilePath'
    | 'lowResOutputFilePath'
    | 'lowResPaddedOutputFilePath'
    | 'publishedActionOutputFilePath'
    | 'publishedLowResFilePath'
    | 'publishedLowResPaddedFilePath'
  >
) {
  const filePath = clip[key];
  if (!filePath) {
    return;
  }

  const normalizedPath = normalizeManagedVideoPath(filePath);
  try {
    resolveManagedVideoAbsolutePath(normalizedPath);
  } catch {
    addFinding(findings, {
      severity: 'error',
      type: 'invalid-generated-file-path',
      entityType: 'media:clip',
      entityId: clip.id,
      filePath: normalizedPath,
      message: `${key} is not under a managed media root.`
    });
    return;
  }

  if (!(await pathExists(normalizedPath))) {
    addFinding(findings, {
      severity: 'warning',
      type: 'missing-generated-file',
      entityType: 'media:clip',
      entityId: clip.id,
      filePath: normalizedPath,
      message: `${key} is missing from disk.`
    });
  }
}

function summarizeFindings(findings: MediaCatalogDiagnosticFinding[]) {
  return findings.reduce(
    (summary, finding) => {
      summary.total += 1;
      if (finding.severity === 'error') {
        summary.errors += 1;
      } else {
        summary.warnings += 1;
      }
      summary.byType[finding.type] = (summary.byType[finding.type] ?? 0) + 1;
      return summary;
    },
    {
      total: 0,
      errors: 0,
      warnings: 0,
      byType: {} as Record<string, number>
    }
  );
}

export async function getMediaCatalogDiagnostics(): Promise<MediaCatalogDiagnostics> {
  const library = await readMediaCatalog();
  const assetIds = new Set(library.videoAssets.map((asset) => asset.id));
  const findings: MediaCatalogDiagnosticFinding[] = [];
  const assetsByNormalizedPath = new Map<string, VideoAsset[]>();

  for (const asset of library.videoAssets) {
    const normalizedPath = normalizeManagedVideoPath(asset.filePath);
    const entries = assetsByNormalizedPath.get(normalizedPath) ?? [];
    entries.push(asset);
    assetsByNormalizedPath.set(normalizedPath, entries);
    await checkAssetFile(findings, asset);
  }

  for (const [filePath, assets] of assetsByNormalizedPath.entries()) {
    if (assets.length <= 1) {
      continue;
    }
    for (const asset of assets) {
      addFinding(findings, {
        severity: 'error',
        type: 'duplicate-asset-file-path',
        entityType: 'media:asset',
        entityId: asset.id,
        filePath,
        relatedId: assets
          .filter((entry) => entry.id !== asset.id)
          .map((entry) => entry.id)
          .join(', '),
        message: 'Multiple video assets resolve to the same managed file path.'
      });
    }
  }

  for (const link of library.moveVideoLinks) {
    if (!assetIds.has(link.assetId)) {
      addFinding(findings, {
        severity: 'error',
        type: 'missing-move-link-asset',
        entityType: 'media:moveVideoLink',
        entityId: link.id,
        relatedId: link.assetId,
        message: 'Move video link points at a missing video asset.'
      });
    }
  }

  for (const clip of library.derivedClips) {
    if (!assetIds.has(clip.sourceAssetId)) {
      addFinding(findings, {
        severity: 'error',
        type: 'missing-clip-source-asset',
        entityType: 'media:clip',
        entityId: clip.id,
        relatedId: clip.sourceAssetId,
        message: 'Derived clip points at a missing source asset.'
      });
    }

    for (const [field, assetId] of [
      ['outputAssetId', clip.outputAssetId],
      ['publishedAssetId', clip.publishedAssetId]
    ] as const) {
      if (assetId && !assetIds.has(assetId)) {
        addFinding(findings, {
          severity: 'error',
          type: 'missing-clip-output-asset',
          entityType: 'media:clip',
          entityId: clip.id,
          relatedId: assetId,
          message: `${field} points at a missing video asset.`
        });
      }
    }

    for (const field of [
      'actionOutputFilePath',
      'lowResOutputFilePath',
      'lowResPaddedOutputFilePath',
      'publishedActionOutputFilePath',
      'publishedLowResFilePath',
      'publishedLowResPaddedFilePath'
    ] as const) {
      await checkClipVariantPath(findings, clip, field);
    }
  }

  findings.sort((left, right) => {
    if (left.severity !== right.severity) {
      return left.severity === 'error' ? -1 : 1;
    }
    if (left.type !== right.type) {
      return left.type.localeCompare(right.type);
    }
    return left.entityId.localeCompare(right.entityId);
  });

  const clipsByStatus: MediaCatalogDiagnostics['counts']['clipsByStatus'] = {
    pending: 0,
    rendering: 0,
    ready: 0,
    failed: 0
  };
  for (const clip of library.derivedClips) {
    clipsByStatus[clip.status] += 1;
  }

  const bootstrapValue = bootstrapMetaValue();
  return {
    checkedAt: new Date().toISOString(),
    database: {
      pathLabel: 'DATA_DIR/app-state.sqlite',
      bootstrapStatus: bootstrapValue === 'complete' ? 'complete' : 'incomplete',
      bootstrapMetaValue: bootstrapValue
    },
    counts: {
      videoAssets: library.videoAssets.length,
      sourceAssets: library.videoAssets.filter((asset) => asset.kind === 'source').length,
      moveAssets: library.videoAssets.filter((asset) => asset.kind === 'move').length,
      moveVideoLinks: library.moveVideoLinks.length,
      derivedClips: library.derivedClips.length,
      clipsByStatus
    },
    latestExport: await latestCatalogExport(),
    integritySummary: summarizeFindings(findings),
    findings
  };
}
