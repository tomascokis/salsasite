import { json } from '@sveltejs/kit';
import { getMoves } from '$lib/server/data';
import { listMoveDrafts } from '$lib/server/move-editor';
import { saveSourceClips } from '$lib/server/video-library';
import type { ClipCountMarker, ClipCropRect, CountOverlayPlacement, CountTimingPreset } from '$lib/types';

const VALID_COUNT_PLACEMENTS = new Set<CountOverlayPlacement>(['top-left', 'top-right', 'bottom-left', 'bottom-right']);
const VALID_COUNT_PRESETS = new Set<CountTimingPreset>(['on2-default', 'on2-all', 'on1-default', 'on1-all']);

function parseCropRect(value: unknown): ClipCropRect | null {
  if (!value || typeof value !== 'object') {
    return null;
  }

  const raw = value as Partial<ClipCropRect>;
  const x = Number(raw.x);
  const y = Number(raw.y);
  const width = Number(raw.width);
  const height = Number(raw.height);
  if (![x, y, width, height].every(Number.isFinite) || width <= 0 || height <= 0) {
    return null;
  }

  return { x, y, width, height };
}

function parseCountMarkers(value: unknown): ClipCountMarker[] {
  if (!Array.isArray(value)) {
    return [];
  }

  return value
    .map((entry) => {
      if (!entry || typeof entry !== 'object') {
        return null;
      }

      const raw = entry as Partial<ClipCountMarker>;
      const count = String(raw.count ?? '').trim();
      const ms = Number(raw.ms ?? 0);
      if (!count || !Number.isFinite(ms)) {
        return null;
      }

      return {
        id: String(raw.id ?? ''),
        count,
        ms,
        clear: Boolean(raw.clear)
      };
    })
    .filter((entry): entry is ClipCountMarker => Boolean(entry));
}

export async function POST({ request }) {
  const body = await request.json();
  const sourceAssetId = String(body.sourceAssetId ?? '').trim();
  const rawClips = Array.isArray(body.clips) ? body.clips : [];

  if (!sourceAssetId) {
    return json({ error: 'sourceAssetId is required.' }, { status: 400 });
  }

  const [moves, moveDrafts] = await Promise.all([getMoves(), listMoveDrafts()]);
  const knownMoveIds = new Set([
    ...moves.map((move) => move.id.toUpperCase()),
    ...moveDrafts.map((draft) => draft.move.id.toUpperCase())
  ]);

  const clips: Array<{
    id?: string;
    moveId: string;
    label: string | null;
    manuallyNamed: boolean;
    startMs: number;
    endMs: number;
    actionStartMs: number | null;
    actionEndMs: number | null;
    cropRect: ClipCropRect | null;
    countMarkers: ClipCountMarker[];
    countOverlayPlacement: CountOverlayPlacement;
    countTimingPreset: CountTimingPreset;
  }> = rawClips
    .map((clip: Record<string, unknown>) => ({
      id: clip.id ? String(clip.id) : undefined,
      moveId: String(clip.moveId ?? '').trim().toUpperCase(),
      label: String(clip.label ?? '').trim() || null,
      manuallyNamed: Boolean(clip.manuallyNamed),
      startMs: Number(clip.startMs ?? 0),
      endMs: Number(clip.endMs ?? 0),
      actionStartMs: clip.actionStartMs === null || clip.actionStartMs === undefined ? null : Number(clip.actionStartMs),
      actionEndMs: clip.actionEndMs === null || clip.actionEndMs === undefined ? null : Number(clip.actionEndMs),
      cropRect: parseCropRect(clip.cropRect),
      countMarkers: parseCountMarkers(clip.countMarkers),
      countOverlayPlacement: VALID_COUNT_PLACEMENTS.has(clip.countOverlayPlacement as CountOverlayPlacement)
        ? (clip.countOverlayPlacement as CountOverlayPlacement)
        : 'top-left',
      countTimingPreset: VALID_COUNT_PRESETS.has(clip.countTimingPreset as CountTimingPreset)
        ? (clip.countTimingPreset as CountTimingPreset)
        : 'on2-default'
    }))
    .filter(
      (clip: {
        moveId: string;
        startMs: number;
        endMs: number;
        actionStartMs: number | null;
        actionEndMs: number | null;
      }) =>
        clip.moveId &&
        knownMoveIds.has(clip.moveId) &&
        Number.isFinite(clip.startMs) &&
        Number.isFinite(clip.endMs) &&
        (clip.actionStartMs === null || Number.isFinite(clip.actionStartMs)) &&
        (clip.actionEndMs === null || Number.isFinite(clip.actionEndMs)) &&
        clip.endMs > clip.startMs
    );

  try {
    const saved = await saveSourceClips({
      sourceAssetId,
      clips
    });

    return json({ ok: true, clips: saved });
  } catch (error) {
    return json(
      { error: error instanceof Error ? error.message : 'Could not save clips.' },
      { status: 400 }
    );
  }
}
