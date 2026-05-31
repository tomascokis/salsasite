<script lang="ts">
  import { browser } from '$app/environment';
  import { onDestroy, tick } from 'svelte';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import { publicationStatusFor, processingStatusFor } from '$lib/content-status';
  import {
    applyVideoAudioPreference,
    hasActiveMutedVideoPreference,
    saveVideoAudioPreferenceFromElement
  } from '$lib/video-audio-preference';
  import { snapMoveBoundaryForDrag } from '$lib/timeline-snapping.js';
  import type {
    ClipCountMarker,
    ClipCropRect,
    CountOverlayPlacement,
    CountTimingPreset,
    DerivedClip,
    PositionOption,
    VideoContentType,
    VideoEnvironment,
    VideoOriginType,
    VideoTiming
  } from '$lib/types';

  type MoveOption = {
    id: string;
    slug: string;
    name: string | null;
    isDraft?: boolean;
  };

  type UploadAssetView = {
    id: string;
    kind: 'source';
    filePath: string;
    displayName: string;
    originalFilename: string;
    dancers: string[];
    timing: VideoTiming;
    contentType: VideoContentType;
    environment: VideoEnvironment;
    originType: VideoOriginType;
    sourceUrl: string | null;
    recordDate: string | null;
    classWorkshop: string | null;
    tags: string[];
    notes: string | null;
    createdAt: string;
    posterFile: string | null;
    linkedMoveIds: string[];
    clips: DerivedClip[];
  };

  type ClipWithUi = DerivedClip & { selected?: boolean };
  type DraftMoveRow = {
    id: string;
    originalClipId?: string | null;
    moveIds: string[];
    query: string;
    descriptorLabel: string;
    startPositionId: string | null;
    startPositionQuery: string;
    endPositionId: string | null;
    endPositionQuery: string;
    timingGroupId: string | null;
    startMs: number;
    endMs: number;
  };
  type EditorMoveRow =
    | { kind: 'draft'; key: string; row: DraftMoveRow }
    | { kind: 'saved'; key: string; clip: ClipWithUi };
  type TimelineMarker = 'clipStart' | 'clipEnd' | 'moveStart' | 'moveEnd' | 'playhead';
  type CountModeStep = 'idle' | 'placing';
  type ClipChangeState = 'new' | 'edited';
  const MOVE_SUGGESTION_LIMIT = 8;
  const CLIP_MOVE_BUFFER_MS = 500;
  const DEFAULT_MOVE_DURATION_MS = 2500;
  const DEFAULT_CLIP_HEAD_PADDING_MS = DEFAULT_MOVE_DURATION_MS;
  const DEFAULT_CLIP_TAIL_PADDING_MS = Math.round(DEFAULT_MOVE_DURATION_MS / 2);
  const MOVE_BOUNDARY_SNAP_TOLERANCE_PX = 8;
  const PLAYBACK_CONTEXT_WINDOW_MS = 2500;
  const COUNT_PRESET_SEQUENCES: Record<CountTimingPreset, string[]> = {
    'on2-default': ['6', '7', '1', '2', '3', '5'],
    'on2-all': ['6', '7', '1', '2', '3', '4', '5', '6', '7', '8'],
    'on1-default': ['1', '2', '3', '5', '6', '7'],
    'on1-all': ['1', '2', '3', '4', '5', '6', '7', '8']
  };

  export let data: {
    timingOptions: Array<{ value: VideoTiming; label: string }>;
    contentTypeOptions: Array<{ value: VideoContentType; label: string }>;
    environmentOptions: Array<{ value: VideoEnvironment; label: string }>;
    dancerOptions: string[];
    positionOptions: PositionOption[];
    moves: MoveOption[];
    assets: UploadAssetView[];
    total: number;
    nextCursor: string | null;
    suggestions: {
      classWorkshops: string[];
      tags: string[];
    };
    selectedAssetId: string;
    selectedClipId: string | null;
  };

  let assets = data.assets;
  let totalAssets = data.total;
  let nextCursor = data.nextCursor;
  let suggestions = data.suggestions;
  let selectedAssetId: string | null = data.selectedAssetId;
  let uploadStatus = '';
  let saveStatus = '';
  let detectStatus = '';
  let renderStatus = '';
  let deleteStatus = '';
  let uploadFile: File | null = null;
  let editDisplayName = '';
  let editDancers = '';
  let editTiming: VideoTiming = 'on2';
  let editContentType: VideoContentType = 'music';
  let editEnvironment: VideoEnvironment = 'class';
  let editOriginType: VideoOriginType = 'self-recorded';
  let editDancerIds: string[] = [];
  let editDancerQuery = '';
  let editSourceUrl = '';
  let editCreatedAt = '';
  let editRecordDate = '';
  let editTags: string[] = [];
  let editTagDraft = '';
  let editNotes = '';
  let isEditingMetadata = false;
  let isUploadingDragOver = false;
  let isLoadingMore = false;
  let clipRows: ClipWithUi[] = [];
  let persistedClipRows: DerivedClip[] = [];
  let activeClipId: string | null = null;
  let isCroppingClip = false;
  let cropDragStart: { x: number; y: number } | null = null;
  let countMode: CountModeStep = 'idle';
  let countModeIndex = 0;
  let isDraftingMove = false;
  let draftMoveRows: DraftMoveRow[] = [];
  let activeDraftMoveRowId: string | null = null;
  let draftInitialSnapshot = '';
  let draftStartMs = 0;
  let draftEndMs = 0;
  let draftActionStartMs = 0;
  let draftActionEndMs = 0;
  let autoClipStart = true;
  let autoClipEnd = true;
  let clipStartContextMs = DEFAULT_CLIP_HEAD_PADDING_MS;
  let clipEndContextMs = DEFAULT_CLIP_TAIL_PADDING_MS;
  let lastDraftBoundaryTarget: TimelineMarker | null = null;
  let videoElement: HTMLVideoElement | null = null;
  let isPlaying = false;
  let isMuted = false;
  let videoVolume = 1;
  let isVolumeOpen = false;
  let isLooping = false;
  let isLoopingWithPadding = true;
  let playbackError = '';
  let playerDurationMs = 0;
  let playerCurrentMs = 0;
  let pendingSeekMs: number | null = null;
  let autoplayedMediaPath: string | null = null;
  let timelineViewportStartMs = 0;
  let timelineViewportEndMs = 0;
  let hasManualTimelineZoom = false;
  let timelineDragTarget: TimelineMarker | null = null;
  let timelineDragPreviousMs: number | null = null;
  let timelineDragSnapConsumed = false;
  let timelineDragCaptureElement: HTMLElement | null = null;
  let timelineDragPointerId: number | null = null;
  let resumePlaybackAfterTimelineDrag = false;
  let timelineElement: HTMLDivElement | null = null;
  let timelineWidthPx = 0;
  let timelineResizeObserver: ResizeObserver | null = null;
  let playbackAnimationFrame: number | null = null;
  let pollTimer: ReturnType<typeof setTimeout> | null = null;
  let syncingAssetKey: string | null = null;
  let syncedMediaPath: string | null = null;
  let selectedAsset: UploadAssetView | null = null;
  let selectedAssetKey: string | null = null;
  let mediaListElement: HTMLDivElement | null = null;
  let mediaGroups: Array<{ month: string; assets: UploadAssetView[] }> = [];
  let availableMoves: MoveOption[] = data.moves;
  let playbackMoveClips: ClipWithUi[] = [];
  let editorMoveRows: EditorMoveRow[] = [];
  let currentPlaybackMove: ClipWithUi | null = null;
  let previousPlaybackMove: ClipWithUi | null = null;
  let nextPlaybackMove: ClipWithUi | null = null;
  let visiblePreviousPlaybackMove: ClipWithUi | null = null;
  let visibleNextPlaybackMove: ClipWithUi | null = null;
  let showPlaybackMoveContext = false;
  let dancerOptions = data.dancerOptions.map((dancer) => ({ id: dancer, label: dancer }));
  let positionPickerOptions = data.positionOptions.map((position) => ({ id: position.id, label: position.label }));

  let moveNameById = new Map<string, string>();
  $: moveNameById = new Map(availableMoves.map((move) => [move.id, move.name ?? move.id]));

  $: selectedAsset = assets.find((asset) => asset.id === selectedAssetId) ?? null;

  $: selectedAssetKey = selectedAsset ? assetSyncKey(selectedAsset) : null;

  $: activeDraftMoveRow = draftMoveRows.find((row) => row.id === activeDraftMoveRowId) ?? null;
  $: activeSavedClip = clipRows.find((clip) => clip.id === activeClipId) ?? null;
  $: activeDraftOriginalClipIds = new Set(draftMoveRows.map((row) => row.originalClipId).filter((id): id is string => Boolean(id)));
  $: activeCountMarkers = activeSavedClip?.countMarkers ?? [];
  $: currentCountMarker = activeSavedClip ? activeCountMarkers[countModeIndex] ?? null : null;
  $: activePreviewCountMarker = activeVisibleCountMarker(activeCountMarkers, playerCurrentMs);
  $: visibleSavedTimelineClips =
    isDraftingMove && activeDraftOriginalClipIds.size
      ? clipRows.filter((clip) => !activeDraftOriginalClipIds.has(clip.id))
      : isDraftingMove && activeClipId ? clipRows.filter((clip) => clip.id !== activeClipId) : clipRows;
  $: savedSnapBoundaryClips =
    isDraftingMove && activeDraftOriginalClipIds.size
      ? clipRows.filter((clip) => !activeDraftOriginalClipIds.has(clip.id))
      : clipRows;
  $: editorMoveRows = editorRowsForDisplay(clipRows, draftMoveRows, isDraftingMove);
  $: playbackMoveClips = sortedPlaybackClips(clipRows);
  $: currentPlaybackMove = playbackMoveClips.find(
    (clip) => playerCurrentMs >= clipActionStartMs(clip) && playerCurrentMs <= clipActionEndMs(clip)
  ) ?? null;
  $: previousPlaybackMove = currentPlaybackMove
    ? playbackMoveClips[playbackMoveClips.findIndex((clip) => clip.id === currentPlaybackMove?.id) - 1] ?? null
    : lastClipBefore(playbackMoveClips, playerCurrentMs);
  $: nextPlaybackMove = currentPlaybackMove
    ? playbackMoveClips[playbackMoveClips.findIndex((clip) => clip.id === currentPlaybackMove?.id) + 1] ?? null
    : playbackMoveClips.find((clip) => clipActionStartMs(clip) > playerCurrentMs) ?? null;
  $: visiblePreviousPlaybackMove =
    previousPlaybackMove && playerCurrentMs - clipActionEndMs(previousPlaybackMove) <= PLAYBACK_CONTEXT_WINDOW_MS
      ? previousPlaybackMove
      : null;
  $: visibleNextPlaybackMove =
    nextPlaybackMove && clipActionStartMs(nextPlaybackMove) - playerCurrentMs <= PLAYBACK_CONTEXT_WINDOW_MS
      ? nextPlaybackMove
      : null;
  $: showPlaybackMoveContext = Boolean(currentPlaybackMove || visiblePreviousPlaybackMove || visibleNextPlaybackMove);
  $: timelineScaleKey = `${isDraftingMove ? 'editing' : 'full'}:${playerDurationMs}:${timelineViewportStartMs}:${timelineViewportEndMs}`;
  $: clipChangeStates = new Map(
    selectedAsset
      ? clipRows
          .map((clip): [string, ClipChangeState | null] => [clip.id, clipChangeState(clip, persistedClipRows)])
          .filter((entry): entry is [string, ClipChangeState] => Boolean(entry[1]))
      : []
  );
  $: hasUnsavedClipRowChanges = selectedAsset ? hasUnsavedClipChanges(persistedClipRows) : false;

  $: selectedDraftMoveIds = draftMoveRows
    .flatMap((row) => row.moveIds)
    .filter(Boolean);

  $: currentDraftSnapshot = JSON.stringify([
    draftMoveRows.map((row) => [
      row.id,
      row.originalClipId ?? '',
      row.moveIds,
      row.query,
      row.descriptorLabel,
      row.startPositionId ?? '',
      row.endPositionId ?? '',
      row.timingGroupId ?? '',
      row.startMs,
      row.endMs
    ]),
    draftStartMs,
    draftEndMs
  ]);

  $: hasDraftChanges = isDraftingMove && currentDraftSnapshot !== draftInitialSnapshot;

  $: hasSaveableDraftChanges =
    hasDraftChanges && draftMoveRows.some((row) => row.moveIds.some((moveId) => moveNameById.has(moveId)) && row.endMs > row.startMs);

  $: if (!isDraftingMove) {
    const timelineDurationMs = inferredTimelineDurationMs();
    if (
      timelineDurationMs &&
      (hasManualTimelineZoom || timelineViewportStartMs !== 0 || timelineViewportEndMs !== timelineDurationMs)
    ) {
      resetTimelineZoom();
    }
  }

  $: if (!isDraftingMove && isLooping) {
    isLooping = false;
  }

  $: if (browser) {
    timelineResizeObserver?.disconnect();
    timelineResizeObserver = null;
    if (timelineElement) {
      syncTimelineWidth();
      timelineResizeObserver = new ResizeObserver(() => {
        syncTimelineWidth();
      });
      timelineResizeObserver.observe(timelineElement);
    } else {
      timelineWidthPx = 0;
    }
  }

  $: if (selectedAsset && selectedAssetKey && syncingAssetKey !== selectedAssetKey) {
    const mediaPathChanged = syncedMediaPath !== selectedAsset.filePath;
    syncingAssetKey = selectedAssetKey;
    syncedMediaPath = selectedAsset.filePath;
    editDisplayName = selectedAsset.displayName;
    editDancers = selectedAsset.dancers.join(', ');
    editDancerIds = [...selectedAsset.dancers];
    editDancerQuery = '';
    editTiming = selectedAsset.timing;
    editContentType = selectedAsset.contentType;
    editEnvironment = selectedAsset.environment;
    editOriginType = selectedAsset.originType;
    editSourceUrl = selectedAsset.sourceUrl ?? '';
    editCreatedAt = dateInputValue(selectedAsset.createdAt);
    editRecordDate = selectedAsset.recordDate ?? '';
    editTags = [...selectedAsset.tags];
    editTagDraft = '';
    editNotes = selectedAsset.notes ?? '';
    detectStatus = '';
    isEditingMetadata = false;
    clipRows = selectedAsset.clips.map((clip) => ({ ...clip, selected: false }));
    persistedClipRows = selectedAsset.clips.map((clip) => ({ ...clip }));
    draftMoveRows = [];
    activeDraftMoveRowId = null;
    activeClipId = null;
    isCroppingClip = false;
    cropDragStart = null;
    countMode = 'idle';
    countModeIndex = 0;
    isDraftingMove = false;
    draftInitialSnapshot = '';
    draftStartMs = 0;
    draftEndMs = playerDurationMs ? Math.min(playerDurationMs, DEFAULT_MOVE_DURATION_MS) : DEFAULT_MOVE_DURATION_MS;
    draftActionStartMs = 0;
    draftActionEndMs = playerDurationMs ? Math.min(playerDurationMs, DEFAULT_MOVE_DURATION_MS) : DEFAULT_MOVE_DURATION_MS;
    autoClipStart = true;
    autoClipEnd = true;
    clipStartContextMs = DEFAULT_CLIP_HEAD_PADDING_MS;
    clipEndContextMs = DEFAULT_CLIP_TAIL_PADDING_MS;
    if (mediaPathChanged) {
      playerDurationMs = 0;
      playerCurrentMs = 0;
      pendingSeekMs = 0;
      autoplayedMediaPath = null;
      timelineViewportStartMs = 0;
      timelineViewportEndMs = 0;
      hasManualTimelineZoom = false;
      void tick().then(initializeSelectedVideo);
    }
    if (data.selectedClipId && clipRows.some((clip) => clip.id === data.selectedClipId)) {
      const clip = clipRows.find((entry) => entry.id === data.selectedClipId);
      if (clip) {
        openSavedClipEditor(clip);
      }
    }
  }

  function mediaUrl(filePath: string) {
    return `/media/${encodeURIComponent(filePath)}`;
  }

  function assetSyncKey(asset: UploadAssetView) {
    const clipSignature = asset.clips
      .map((clip) =>
        [
          clip.id,
          clip.moveId,
          clip.isKeyVideo ? 'key' : 'normal',
          clip.label ?? '',
          clip.descriptorLabel ?? '',
          clip.startPositionId ?? '',
          clip.endPositionId ?? '',
          clip.timingGroupId ?? '',
          clip.startMs,
          clip.endMs,
          clip.actionStartMs ?? '',
          clip.actionEndMs ?? '',
          clip.cropRect ? `${clip.cropRect.x},${clip.cropRect.y},${clip.cropRect.width},${clip.cropRect.height}` : '',
          clip.countMarkers.map((marker) => `${marker.id},${marker.count},${marker.ms},${marker.clear}`).join(';'),
          clip.countOverlayPlacement,
          clip.countTimingPreset,
          clip.outputAssetId ?? '',
          clip.lowResOutputFilePath ?? '',
          clip.lowResPaddedOutputFilePath ?? '',
          clip.publishedAssetId ?? '',
          clip.publishedLowResFilePath ?? '',
          clip.publishedLowResPaddedFilePath ?? '',
          clip.publishedAt ?? '',
          clip.manuallyNamed ? 'manual' : 'generated',
          clip.status,
          clip.error ?? ''
        ].join(':')
      )
      .join('|');

    return [
      asset.id,
      asset.filePath,
      asset.displayName,
      asset.dancers.join(','),
      asset.timing,
      asset.contentType,
      asset.environment,
      asset.originType,
      asset.sourceUrl ?? '',
      asset.createdAt,
      asset.recordDate ?? '',
      asset.classWorkshop ?? '',
      asset.tags.join(','),
      asset.notes ?? '',
      asset.linkedMoveIds.join(','),
      clipSignature
    ].join('::');
  }

  function formatSeconds(milliseconds: number) {
    return (milliseconds / 1000).toFixed(2);
  }

  function formatTenthSeconds(milliseconds: number) {
    return (milliseconds / 1000).toFixed(1);
  }

  function formatRoundedSeconds(milliseconds: number) {
    return String(Math.round(milliseconds / 1000));
  }

  function timingLabel(value: VideoTiming) {
    return data.timingOptions.find((entry) => entry.value === value)?.label ?? value;
  }

  function contentTypeLabel(value: VideoContentType) {
    return data.contentTypeOptions.find((entry) => entry.value === value)?.label ?? value;
  }

  function environmentLabel(value: VideoEnvironment) {
    return data.environmentOptions.find((entry) => entry.value === value)?.label ?? value;
  }

  function normalizeMoveId(value: string) {
    return value.trim().toUpperCase();
  }

  function updateDraftMoveRow(rowId: string, patch: Partial<DraftMoveRow>) {
    const target = draftMoveRows.find((row) => row.id === rowId);
    const timingPatch = {
      ...(patch.startMs !== undefined ? { startMs: patch.startMs } : {}),
      ...(patch.endMs !== undefined ? { endMs: patch.endMs } : {})
    };
    const shouldSyncTiming = target?.timingGroupId && (patch.startMs !== undefined || patch.endMs !== undefined);

    draftMoveRows = draftMoveRows.map((row) => {
      if (row.id === rowId) {
        return { ...row, ...patch };
      }
      if (shouldSyncTiming && row.timingGroupId === target?.timingGroupId) {
        return { ...row, ...timingPatch };
      }
      return row;
    });
  }

  function createDraftMoveRow(startMs: number, endMs: number, timingGroupId: string | null = null): DraftMoveRow {
    return {
      id: createDraftClipId(),
      originalClipId: null,
      moveIds: [],
      query: '',
      descriptorLabel: '',
      startPositionId: null,
      startPositionQuery: '',
      endPositionId: null,
      endPositionQuery: '',
      timingGroupId,
      startMs,
      endMs
    };
  }

  function syncActiveDraftMoveRowRange() {
    if (!activeDraftMoveRowId) {
      return;
    }

    updateDraftMoveRow(activeDraftMoveRowId, {
      startMs: draftActionStartMs,
      endMs: draftActionEndMs
    });
  }

  function selectDraftMoveRow(rowId: string) {
    const row = draftMoveRows.find((entry) => entry.id === rowId);
    if (!row) {
      return;
    }

    activeDraftMoveRowId = row.id;
    draftActionStartMs = row.startMs;
    draftActionEndMs = row.endMs;
    draftStartMs = clampClipStartMs(Math.max(0, draftActionStartMs - Math.max(clipStartContextMs, CLIP_MOVE_BUFFER_MS)));
    draftEndMs = clampClipEndMs(draftActionEndMs + Math.max(clipEndContextMs, CLIP_MOVE_BUFFER_MS));
    seekPreview(draftActionStartMs);
  }

  function addDraftMove(moveId: string, rowId = activeDraftMoveRowId) {
    const normalized = normalizeMoveId(moveId);
    if (!rowId || !normalized || !moveNameById.has(normalized) || selectedDraftMoveIds.includes(normalized)) {
      return;
    }

    const row = draftMoveRows.find((entry) => entry.id === rowId);
    if (!row) {
      return;
    }

    updateDraftMoveRow(rowId, {
      moveIds: [normalized],
      query: ''
    });
  }

  function removeDraftMove(rowId: string, moveId: string) {
    const row = draftMoveRows.find((entry) => entry.id === rowId);
    if (!row) {
      return;
    }

    updateDraftMoveRow(rowId, {
      moveIds: row.moveIds.filter((entry) => entry !== moveId),
      query: ''
    });
    activeDraftMoveRowId = rowId;
  }

  function handleDraftMoveQueryInput(rowId: string, value: string) {
    updateDraftMoveRow(rowId, { query: value });
    activeDraftMoveRowId = rowId;
  }

  function updateDraftDescriptor(rowId: string, value: string) {
    updateDraftMoveRow(rowId, { descriptorLabel: value });
    activeDraftMoveRowId = rowId;
  }

  function updateDraftPositionQuery(rowId: string, field: 'start' | 'end', value: string) {
    updateDraftMoveRow(rowId, field === 'start' ? { startPositionQuery: value } : { endPositionQuery: value });
    activeDraftMoveRowId = rowId;
  }

  function selectDraftPosition(rowId: string, field: 'start' | 'end', id: string) {
    updateDraftMoveRow(
      rowId,
      field === 'start'
        ? { startPositionId: id, startPositionQuery: '' }
        : { endPositionId: id, endPositionQuery: '' }
    );
    activeDraftMoveRowId = rowId;
  }

  function removeDraftPosition(rowId: string, field: 'start' | 'end') {
    updateDraftMoveRow(
      rowId,
      field === 'start'
        ? { startPositionId: null, startPositionQuery: '' }
        : { endPositionId: null, endPositionQuery: '' }
    );
    activeDraftMoveRowId = rowId;
  }

  function positionLabel(positionId: string | null | undefined) {
    if (!positionId) return null;
    return positionPickerOptions.find((option) => option.id === positionId)?.label ?? positionId;
  }

  async function createDraftMoveFromQuery(rowId: string, value: string) {
    const name = value.trim();
    if (!name) {
      return;
    }

    activeDraftMoveRowId = rowId;
    renderStatus = `Creating draft move "${name}"...`;
    const response = await fetch('/api/moves/create', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        action: 'createDraftFromName',
        name
      })
    });
    const payload = await response.json();

    if (!response.ok) {
      renderStatus = payload.error ?? 'Could not create draft move.';
      return;
    }

    const draftMove = payload.draft?.move;
    if (!draftMove?.id) {
      renderStatus = 'Draft move created, but the response did not include a move id.';
      return;
    }

    const option: MoveOption = {
      id: String(draftMove.id),
      slug: String(draftMove.slug ?? draftMove.id),
      name: draftMove.name ?? draftMove.id,
      isDraft: true
    };
    availableMoves = [option, ...availableMoves.filter((move) => move.id !== option.id)];

    const row = draftMoveRows.find((entry) => entry.id === rowId);
    if (!row) {
      renderStatus = `Draft move "${option.name ?? option.id}" created.`;
      return;
    }

    updateDraftMoveRow(rowId, {
      moveIds: [option.id],
      query: ''
    });
    renderStatus = `Draft move "${option.name ?? option.id}" created and selected.`;
  }

  function videoMetaLabel(asset: UploadAssetView) {
    return `${timingLabel(asset.timing)} · ${contentTypeLabel(asset.contentType)} · ${environmentLabel(asset.environment)}`;
  }

  function timingBadgeClass(value: VideoTiming) {
    return `badge-timing-${value}`;
  }

  function contentBadgeClass(value: VideoContentType) {
    return `badge-content-${value}`;
  }

  function environmentBadgeClass(value: VideoEnvironment) {
    return `badge-environment-${value}`;
  }

  function clipPublicationStatus(clip: DerivedClip) {
    return publicationStatusFor({
      isModern: true,
      publishedAt: clip.publishedAt,
      updatedAt: clip.updatedAt
    });
  }

  function clipPublicationClass(clip: DerivedClip) {
    const status = clipPublicationStatus(clip);
    if (status === 'never-published') return 'clip-row-never-published';
    if (status === 'changed-unpublished') return 'clip-row-changed';
    return '';
  }

  function clipDisplayName(clip: DerivedClip) {
    const base = clip.label?.trim() || moveNameById.get(clip.moveId) || clip.moveId;
    return clip.descriptorLabel?.trim() ? `${base} - ${clip.descriptorLabel.trim()}` : base;
  }

  function playbackMoveName(clip: DerivedClip) {
    return moveNameById.get(clip.moveId) || clip.moveId;
  }

  function clipActionStartMs(clip: DerivedClip) {
    return clip.actionStartMs ?? clip.startMs;
  }

  function clipActionEndMs(clip: DerivedClip) {
    return clip.actionEndMs ?? clip.endMs;
  }

  function sortedPlaybackClips(clips: ClipWithUi[]) {
    return [...clips].sort((left, right) => {
      const startDelta = clipActionStartMs(left) - clipActionStartMs(right);
      return startDelta || clipActionEndMs(left) - clipActionEndMs(right) || left.id.localeCompare(right.id);
    });
  }

  function editorRowsForDisplay(clips: ClipWithUi[], rows: DraftMoveRow[], editing: boolean): EditorMoveRow[] {
    if (!editing) {
      return clips.map((clip) => ({ kind: 'saved', key: `saved-${clip.id}`, clip }));
    }

    const draftByOriginalClipId = new Map<string, DraftMoveRow>();
    const newDraftRows: DraftMoveRow[] = [];
    rows.forEach((row) => {
      if (row.originalClipId) {
        draftByOriginalClipId.set(row.originalClipId, row);
      } else {
        newDraftRows.push(row);
      }
    });

    const result: EditorMoveRow[] = clips.map((clip) => {
      const draftRow = draftByOriginalClipId.get(clip.id);
      return draftRow
        ? { kind: 'draft', key: `draft-${draftRow.id}`, row: draftRow }
        : { kind: 'saved', key: `saved-${clip.id}`, clip };
    });

    newDraftRows.forEach((row) => {
      const draftEntry: EditorMoveRow = { kind: 'draft', key: `draft-${row.id}`, row };
      const insertIndex = result.findIndex((entry) => editorRowStartMs(entry) > row.startMs);
      if (insertIndex < 0) {
        result.push(draftEntry);
      } else {
        result.splice(insertIndex, 0, draftEntry);
      }
    });

    return result;
  }

  function editorRowStartMs(row: EditorMoveRow) {
    return row.kind === 'draft' ? row.row.startMs : clipActionStartMs(row.clip);
  }

  function lastClipBefore(clips: ClipWithUi[], milliseconds: number) {
    for (let index = clips.length - 1; index >= 0; index -= 1) {
      if (clipActionEndMs(clips[index]) < milliseconds) {
        return clips[index];
      }
    }

    return null;
  }

  function clipSaveSignature(clip: DerivedClip) {
    return JSON.stringify([
      clip.moveId,
      clip.isKeyVideo ? 'key' : 'normal',
      clip.label ?? '',
      clip.descriptorLabel ?? '',
      clip.startPositionId ?? '',
      clip.endPositionId ?? '',
      clip.timingGroupId ?? '',
      clip.manuallyNamed ? 'manual' : 'generated',
      clip.startMs,
      clip.endMs,
      clip.actionStartMs ?? '',
      clip.actionEndMs ?? '',
      clip.cropRect ? [clip.cropRect.x, clip.cropRect.y, clip.cropRect.width, clip.cropRect.height] : null,
      clip.countMarkers.map((marker) => [marker.id, marker.count, marker.ms, marker.clear]),
      clip.countOverlayPlacement,
      clip.countTimingPreset
    ]);
  }

  function clipChangeState(clip: DerivedClip, savedClips: DerivedClip[]): ClipChangeState | null {
    const savedClip = savedClips.find((entry) => entry.id === clip.id);
    if (!savedClip) return 'new';
    return clipSaveSignature(clip) === clipSaveSignature(savedClip) ? null : 'edited';
  }

  function hasUnsavedClipChanges(savedClips: DerivedClip[]) {
    if (clipRows.length !== savedClips.length) return true;
    const currentIds = new Set(clipRows.map((clip) => clip.id));
    if (savedClips.some((clip) => !currentIds.has(clip.id))) return true;
    return clipRows.some((clip) => Boolean(clipChangeState(clip, savedClips)));
  }

  function clipChangeLabel(state: ClipChangeState | null | undefined) {
    if (state === 'new') return 'New';
    if (state === 'edited') return 'Edited';
    return 'Saved';
  }

  function updateClipLabel(clipId: string, value: string) {
    clipRows = clipRows.map((clip) =>
      clip.id === clipId ? { ...clip, label: value, manuallyNamed: true, updatedAt: new Date().toISOString() } : clip
    );
  }

  function toggleClipKeyVideo(clipId: string) {
    clipRows = clipRows.map((clip) =>
      clip.id === clipId ? { ...clip, isKeyVideo: !clip.isKeyVideo, updatedAt: new Date().toISOString() } : clip
    );
  }

  function createUiId(prefix: string) {
    if (typeof globalThis.crypto?.randomUUID === 'function') {
      return globalThis.crypto.randomUUID();
    }

    return `${prefix}-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
  }

  function updateActiveClip(patch: Partial<DerivedClip>) {
    if (!activeClipId) {
      return;
    }

    clipRows = clipRows.map((clip) =>
      clip.id === activeClipId ? { ...clip, ...patch, updatedAt: new Date().toISOString() } : clip
    );
  }

  function selectSavedClip(clip: DerivedClip) {
    activeClipId = clip.id;
    countMode = 'idle';
    countModeIndex = 0;
    seekPreview(clipActionStartMs(clip));
  }

  function openSavedClipEditor(clip: DerivedClip) {
    const actionStart = clipActionStartMs(clip);
    const actionEnd = clipActionEndMs(clip);
    const groupClips = clip.timingGroupId
      ? clipRows.filter((entry) => entry.timingGroupId === clip.timingGroupId)
      : [clip];
    const rows = groupClips.map((entry) => ({
      ...createDraftMoveRow(clipActionStartMs(entry), clipActionEndMs(entry), entry.timingGroupId ?? null),
      originalClipId: entry.id,
      moveIds: [entry.moveId],
      query: '',
      descriptorLabel: entry.descriptorLabel ?? '',
      startPositionId: entry.startPositionId ?? null,
      startPositionQuery: '',
      endPositionId: entry.endPositionId ?? null,
      endPositionQuery: ''
    }));
    const row = rows.find((entry) => entry.originalClipId === clip.id) ?? rows[0];

    activeClipId = clip.id;
    isDraftingMove = true;
    draftMoveRows = rows;
    activeDraftMoveRowId = row.id;
    draftActionStartMs = actionStart;
    draftActionEndMs = actionEnd;
    draftStartMs = clampClipStartMs(clip.startMs);
    draftEndMs = clampClipEndMs(clip.endMs);
    clipStartContextMs = Math.max(CLIP_MOVE_BUFFER_MS, actionStart - clip.startMs);
    clipEndContextMs = Math.max(CLIP_MOVE_BUFFER_MS, clip.endMs - actionEnd);
    autoClipStart = false;
    autoClipEnd = false;
    isLooping = true;
    isLoopingWithPadding = true;
    countMode = 'idle';
    countModeIndex = 0;
    isCroppingClip = false;
    resetTimelineZoom();
    draftInitialSnapshot = JSON.stringify([
      draftMoveRows.map((draftRow) => [
        draftRow.id,
        draftRow.originalClipId ?? '',
        draftRow.moveIds,
        draftRow.query,
        draftRow.descriptorLabel,
        draftRow.startPositionId ?? '',
        draftRow.endPositionId ?? '',
        draftRow.timingGroupId ?? '',
        draftRow.startMs,
        draftRow.endMs
      ]),
      draftStartMs,
      draftEndMs
    ]);
    seekPreview(actionStart);
  }

  function removeSavedClip(clipId: string) {
    clipRows = clipRows.filter((clip) => clip.id !== clipId);
    if (activeClipId === clipId) {
      activeClipId = null;
      isCroppingClip = false;
      cropDragStart = null;
      countMode = 'idle';
      countModeIndex = 0;
    }
    renderStatus = 'Move track removed. Save clip changes to persist.';
  }

  function pointerRatio(event: PointerEvent, element: HTMLElement) {
    const bounds = element.getBoundingClientRect();
    return {
      x: Math.max(0, Math.min(1, (event.clientX - bounds.left) / bounds.width)),
      y: Math.max(0, Math.min(1, (event.clientY - bounds.top) / bounds.height))
    };
  }

  function cropRectStyle(cropRect: ClipCropRect | null) {
    if (!cropRect) return '';
    return [
      `left: ${cropRect.x * 100}%`,
      `top: ${cropRect.y * 100}%`,
      `width: ${cropRect.width * 100}%`,
      `height: ${cropRect.height * 100}%`
    ].join('; ');
  }

  function startCropDrag(event: PointerEvent) {
    if (!isCroppingClip || !activeSavedClip) {
      return;
    }

    event.preventDefault();
    event.stopPropagation();
    const target = event.currentTarget as HTMLElement;
    target.setPointerCapture?.(event.pointerId);
    cropDragStart = pointerRatio(event, target);
    updateActiveClip({ cropRect: { ...cropDragStart, width: 0.02, height: 0.02 } });
  }

  function updateCropDrag(event: PointerEvent) {
    if (!cropDragStart || !isCroppingClip || !activeSavedClip) {
      return;
    }

    event.preventDefault();
    const current = pointerRatio(event, event.currentTarget as HTMLElement);
    const x = Math.min(cropDragStart.x, current.x);
    const y = Math.min(cropDragStart.y, current.y);
    const width = Math.max(0.02, Math.abs(current.x - cropDragStart.x));
    const height = Math.max(0.02, Math.abs(current.y - cropDragStart.y));
    updateActiveClip({ cropRect: { x, y, width: Math.min(1 - x, width), height: Math.min(1 - y, height) } });
  }

  function finishCropDrag() {
    cropDragStart = null;
  }

  function clearActiveCrop() {
    updateActiveClip({ cropRect: null });
    isCroppingClip = false;
  }

  function presetLabel(value: CountTimingPreset) {
    if (value === 'on2-default') return 'On2';
    if (value === 'on2-all') return 'On2 all';
    if (value === 'on1-default') return 'On1';
    return 'On1 all';
  }

  function countPlacementLabel(value: CountOverlayPlacement) {
    if (value === 'top-left') return 'Top left';
    if (value === 'top-right') return 'Top right';
    if (value === 'bottom-left') return 'Bottom left';
    return 'Bottom right';
  }

  function countSequenceFor(preset: CountTimingPreset) {
    return COUNT_PRESET_SEQUENCES[preset];
  }

  function buildCountMarkers(preset: CountTimingPreset, clip: DerivedClip) {
    const startMs = clip.actionStartMs ?? clip.startMs;
    return countSequenceFor(preset).map((count, index) => ({
      id: createUiId('count'),
      count,
      ms: startMs,
      clear: false
    }));
  }

  function setActiveCountPreset(preset: CountTimingPreset) {
    if (!activeSavedClip) {
      return;
    }

    updateActiveClip({
      countTimingPreset: preset,
      countMarkers: buildCountMarkers(preset, activeSavedClip)
    });
    countModeIndex = 0;
  }

  function setActiveCountPlacement(placement: CountOverlayPlacement) {
    updateActiveClip({ countOverlayPlacement: placement });
  }

  function zoomTimelineToClip(clip: DerivedClip) {
    const startMs = Math.max(0, (clip.actionStartMs ?? clip.startMs) - 2000);
    const endMs = Math.min(inferredTimelineDurationMs(), (clip.actionEndMs ?? clip.endMs) + 2000);
    timelineViewportStartMs = startMs;
    timelineViewportEndMs = Math.max(endMs, startMs + 2000);
    hasManualTimelineZoom = true;
  }

  function startCountMode() {
    if (!activeSavedClip) {
      renderStatus = 'Select a move clip first.';
      return;
    }

    if (!activeSavedClip.countMarkers.length) {
      updateActiveClip({
        countMarkers: buildCountMarkers(activeSavedClip.countTimingPreset, activeSavedClip)
      });
    }

    countMode = 'placing';
    countModeIndex = 0;
    zoomTimelineToClip(activeSavedClip);
    seekPreview(activeSavedClip.actionStartMs ?? activeSavedClip.startMs);
  }

  function finishCountMode() {
    countMode = 'idle';
    countModeIndex = 0;
  }

  function placeCurrentCount() {
    if (!activeSavedClip || !currentCountMarker) {
      return;
    }

    const markerId = currentCountMarker.id;
    updateActiveClip({
      countMarkers: activeSavedClip.countMarkers.map((marker) =>
        marker.id === markerId ? { ...marker, ms: clampMs(playerCurrentMs), clear: false } : marker
      )
    });
    countModeIndex = Math.min(activeSavedClip.countMarkers.length - 1, countModeIndex + 1);
  }

  function toggleCountClear(markerId: string) {
    if (!activeSavedClip) {
      return;
    }

    updateActiveClip({
      countMarkers: activeSavedClip.countMarkers.map((marker) =>
        marker.id === markerId ? { ...marker, clear: !marker.clear } : marker
      )
    });
  }

  function clearCountMarkers() {
    updateActiveClip({ countMarkers: [] });
    countMode = 'idle';
    countModeIndex = 0;
  }

  function activeVisibleCountMarker(markers: ClipCountMarker[], currentMs: number) {
    const sorted = [...markers].sort((left, right) => left.ms - right.ms);
    let visible: ClipCountMarker | null = null;
    for (const marker of sorted) {
      if (marker.ms > currentMs) {
        break;
      }
      visible = marker.clear ? null : marker;
    }
    return visible;
  }

  function countOverlayClass(placement: CountOverlayPlacement) {
    return `count-overlay count-overlay-${placement}`;
  }

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }

  function formatDate(value: string | null) {
    if (!value) return 'Not recorded';
    const date = new Date(value);
    if (Number.isNaN(date.getTime())) return value;
    return new Intl.DateTimeFormat('en-AU', { day: '2-digit', month: 'short', year: 'numeric' }).format(date);
  }

  function dateInputValue(value: string | null) {
    if (!value) return '';
    const text = String(value);
    if (/^\d{4}-\d{2}-\d{2}/.test(text)) {
      return text.slice(0, 10);
    }
    const date = new Date(text);
    return Number.isNaN(date.getTime()) ? '' : date.toISOString().slice(0, 10);
  }

  function sourceUrlLabel(value: string) {
    try {
      const url = new URL(value);
      const host = url.hostname.replace(/^www\./, '');
      if (host.includes('instagram.com')) return 'instagram';
      if (host.includes('youtube.com') || host.includes('youtu.be')) return 'youtube';
      return host;
    } catch {
      return value;
    }
  }

  function monthLabel(value: string) {
    const date = new Date(value);
    if (Number.isNaN(date.getTime())) return 'Unknown upload month';
    return new Intl.DateTimeFormat('en-AU', { month: 'long', year: 'numeric' }).format(date);
  }

  $: mediaGroups = assets.reduce(
    (groups, asset) => {
      const month = monthLabel(asset.createdAt);
      let group = groups.find((entry) => entry.month === month);
      if (!group) {
        group = { month, assets: [] as UploadAssetView[] };
        groups.push(group);
      }
      group.assets.push(asset);
      return groups;
    },
    [] as Array<{ month: string; assets: UploadAssetView[] }>
  );

  function addUniqueTag(tags: string[], value: string) {
    const nextValue = value.trim();
    if (!nextValue) return tags;
    const exists = tags.some((tag) => tag.toLocaleLowerCase() === nextValue.toLocaleLowerCase());
    return exists ? tags : [...tags, nextValue];
  }

  function handleTagDraftKeydown(event: KeyboardEvent) {
    if (event.key !== 'Enter' && event.key !== ',') {
      return;
    }

    event.preventDefault();
    editTags = addUniqueTag(editTags, editTagDraft.replace(/,$/, ''));
    editTagDraft = '';
  }

  function removeTag(tag: string) {
    editTags = editTags.filter((entry) => entry !== tag);
  }

  function addSelectedDancer(dancerId: string) {
    if (!dancerId || editDancerIds.includes(dancerId)) {
      return;
    }
    editDancerIds = [...editDancerIds, dancerId];
    editDancerQuery = '';
  }

  function removeSelectedDancer(dancerId: string) {
    editDancerIds = editDancerIds.filter((entry) => entry !== dancerId);
  }

  function selectAsset(assetId: string) {
    selectedAssetId = assetId;
    syncingAssetKey = null;
    stopPolling();
  }

  async function refreshLibrary(
    nextSelectedAssetId: string | null = selectedAssetId,
    limit = Math.max(50, assets.length || 50)
  ) {
    void limit;
    const response = await fetch('/api/upload/library');
    if (!response.ok) {
      return;
    }

    const payload = await response.json();
    assets = payload.assets;
    totalAssets = payload.assets.length;
    nextCursor = null;
    if (nextSelectedAssetId && assets.some((asset: UploadAssetView) => asset.id === nextSelectedAssetId)) {
      selectedAssetId = nextSelectedAssetId;
      syncingAssetKey = null;
    } else {
      selectedAssetId = assets[0]?.id ?? null;
      syncingAssetKey = null;
    }
  }

  async function loadMoreMedia() {
    if (!nextCursor || isLoadingMore) {
      return;
    }

    isLoadingMore = true;
    const response = await fetch(
      `/api/media/library?limit=50&cursor=${encodeURIComponent(nextCursor)}`
    );

    if (response.ok) {
      const payload = await response.json();
      const existingIds = new Set(assets.map((asset) => asset.id));
      assets = [...assets, ...payload.assets.filter((asset: UploadAssetView) => !existingIds.has(asset.id))];
      totalAssets = payload.total;
      nextCursor = payload.nextCursor;
      suggestions = payload.suggestions;
    }

    isLoadingMore = false;
  }

  function handleMediaListScroll() {
    if (!mediaListElement) {
      return;
    }

    const remaining =
      mediaListElement.scrollHeight - mediaListElement.scrollTop - mediaListElement.clientHeight;
    if (remaining < 420) {
      void loadMoreMedia();
    }
  }

  function defaultDisplayNameFor(file: File) {
    return file.name.replace(/\.[^.]+$/, '').replace(/[_-]+/g, ' ').trim() || file.name;
  }

  function handleUploadDrop(event: DragEvent) {
    event.preventDefault();
    isUploadingDragOver = false;
    const file = event.dataTransfer?.files?.[0] ?? null;
    if (file) {
      void uploadSourceFile(file);
    }
  }

  async function uploadSourceFile(file: File | null) {
    if (!file) {
      uploadStatus = 'Choose a source video first.';
      return;
    }

    uploadFile = file;
    uploadStatus = 'Uploading...';

    const formData = new FormData();
    formData.set('file', file);
    formData.set('displayName', defaultDisplayNameFor(file));
    formData.set('dancers', '');
    formData.set('timing', 'on2');
    formData.set('contentType', 'music');
    formData.set('environment', 'class');
    formData.set('originType', 'self-recorded');
    formData.set('sourceUrl', '');
    formData.set('recordDate', '');
    formData.set('tags', '');
    formData.set('notes', '');

    const response = await fetch('/api/upload/source', {
      method: 'POST',
      body: formData
    });
    const payload = await response.json();

    if (!response.ok) {
      uploadStatus = payload.error ?? 'Upload failed.';
      uploadFile = null;
      return;
    }

    window.location.href = `/media/edit/${encodeURIComponent(payload.asset.id)}`;
  }

  async function saveSourceMetadata() {
    if (!selectedAsset) {
      return;
    }

    if (editTagDraft.trim()) {
      editTags = addUniqueTag(editTags, editTagDraft);
      editTagDraft = '';
    }

    saveStatus = 'Saving metadata...';
    const response = await fetch(`/api/upload/source/${selectedAsset.id}`, {
      method: 'PUT',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        displayName: editDisplayName,
        dancers: editDancerIds,
        timing: editTiming,
        contentType: editContentType,
        environment: editEnvironment,
        originType: editOriginType,
        sourceUrl: editSourceUrl,
        createdAt: editCreatedAt,
        recordDate: editRecordDate,
        tags: editTags,
        notes: editNotes
      })
    });
    const payload = await response.json();

    if (!response.ok) {
      saveStatus = payload.error ?? 'Could not save metadata.';
      return;
    }

    saveStatus = 'Metadata saved.';
    isEditingMetadata = false;
    await refreshLibrary(selectedAsset.id);
  }

  async function detectSourceFields() {
    if (!selectedAsset) {
      return;
    }

    detectStatus = 'Detecting...';
    const response = await fetch(`/api/upload/source/${selectedAsset.id}/detect-fields`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        originType: editOriginType,
        sourceUrl: editSourceUrl
      })
    });
    const payload = await response.json();

    if (!response.ok) {
      detectStatus = payload.error ?? 'Could not detect fields.';
      return;
    }

    if (payload.recordDate) {
      editRecordDate = payload.recordDate;
    }
    detectStatus = payload.message ?? 'Detection complete.';
  }

  async function deleteSourceVideo(asset: UploadAssetView) {
    if (
      browser &&
      !window.confirm(
        `Delete "${asset.displayName}"? This will also remove ${asset.clips.length} clip definition${
          asset.clips.length === 1 ? '' : 's'
        } and any rendered clips made from it.`
      )
    ) {
      return;
    }

    deleteStatus = 'Deleting...';
    const response = await fetch(`/api/upload/source/${asset.id}`, {
      method: 'DELETE'
    });
    const payload = await response.json();

    if (!response.ok) {
      deleteStatus = payload.error ?? 'Could not delete video.';
      return;
    }

    deleteStatus = 'Deleted.';
    window.location.href = '/media';
  }

  function inferredTimelineDurationMs() {
    const clipMax = clipRows.reduce(
      (max, clip) => Math.max(max, clip.endMs, clip.actionEndMs ?? 0, clip.startMs, clip.actionStartMs ?? 0),
      0
    );
    const draftMoveMax = draftMoveRows.reduce((max, row) => Math.max(max, row.startMs, row.endMs), 0);

    return Math.max(playerDurationMs, playerCurrentMs, draftEndMs, draftActionEndMs, draftMoveMax, clipMax);
  }

  function clampMs(value: number) {
    const max = inferredTimelineDurationMs() || Math.max(value, 0);
    return Math.max(0, Math.min(max, Math.round(value)));
  }

  function clampClipStartMs(value: number) {
    return Math.min(clampMs(value), Math.max(0, draftActionStartMs - CLIP_MOVE_BUFFER_MS));
  }

  function clampClipEndMs(value: number) {
    const max = inferredTimelineDurationMs() || Math.max(value, draftActionEndMs + CLIP_MOVE_BUFFER_MS);
    return Math.max(clampMs(value), Math.min(max, draftActionEndMs + CLIP_MOVE_BUFFER_MS));
  }

  function ensureTimelineViewport() {
    const timelineDurationMs = inferredTimelineDurationMs();
    if (!timelineDurationMs) {
      timelineViewportStartMs = 0;
      timelineViewportEndMs = 0;
      return;
    }

    if (!isDraftingMove) {
      timelineViewportStartMs = 0;
      timelineViewportEndMs = timelineDurationMs;
      return;
    }

    if (timelineViewportEndMs <= timelineViewportStartMs) {
      timelineViewportStartMs = 0;
      timelineViewportEndMs = timelineDurationMs;
      return;
    }

    if (timelineViewportEndMs > timelineDurationMs) {
      timelineViewportEndMs = timelineDurationMs;
    }
  }

  function syncVideoMetadata() {
    const durationSeconds = videoElement?.duration;
    if (!Number.isFinite(durationSeconds) || !durationSeconds || durationSeconds <= 0) {
      return false;
    }

    const nextDurationMs = Math.round(durationSeconds * 1000);
    playerDurationMs = nextDurationMs;
    playerCurrentMs = Math.round((videoElement?.currentTime ?? 0) * 1000);

    if (
      !hasManualTimelineZoom ||
      !timelineViewportEndMs ||
      timelineViewportEndMs > nextDurationMs ||
      timelineViewportEndMs <= timelineViewportStartMs
    ) {
      timelineViewportStartMs = 0;
      timelineViewportEndMs = nextDurationMs;
    }

    if (!isDraftingMove) {
      draftEndMs = Math.min(nextDurationMs, Math.max(draftEndMs, DEFAULT_MOVE_DURATION_MS));
      draftActionEndMs = Math.min(nextDurationMs, Math.max(draftActionEndMs, DEFAULT_MOVE_DURATION_MS));
    }

    return true;
  }

  function applyPendingSeek() {
    if (pendingSeekMs === null || !videoElement || !syncVideoMetadata()) {
      return false;
    }

    const nextValue = clampMs(pendingSeekMs);
    try {
      videoElement.currentTime = nextValue / 1000;
      playerCurrentMs = nextValue;
      pendingSeekMs = null;
      return true;
    } catch {
      return false;
    }
  }

  async function initializeSelectedVideo() {
    if (!selectedAsset || !videoElement) {
      return;
    }

    playbackError = '';
    syncVideoMetadata();
    applyPendingSeek();

    if (autoplayedMediaPath === selectedAsset.filePath) {
      return;
    }

    autoplayedMediaPath = selectedAsset.filePath;
    applyVideoAudioPreference(videoElement);
    syncAudioState();

    try {
      await videoElement.play();
    } catch {
      // Browser autoplay policy can still reject this; manual play remains available.
    }
  }

  function syncAudioState() {
    if (!videoElement) {
      return;
    }

    isMuted = videoElement.muted;
    videoVolume = videoElement.volume;
  }

  function handleAudioPreferenceChange() {
    if (!videoElement) {
      return;
    }

    saveVideoAudioPreferenceFromElement(videoElement);
    syncAudioState();
  }

  function handleVolumeInput(event: Event) {
    if (!videoElement) {
      return;
    }

    const nextVolume = Number((event.currentTarget as HTMLInputElement).value);
    if (!Number.isFinite(nextVolume)) {
      return;
    }

    const clampedVolume = Math.max(0, Math.min(1, nextVolume));
    videoElement.volume = clampedVolume;
    videoElement.muted = videoElement.volume === 0;
    handleAudioPreferenceChange();
  }

  function toggleVolumeOpen() {
    isVolumeOpen = !isVolumeOpen;
  }

  function enableMoveEditorAudio() {
    if (!videoElement || hasActiveMutedVideoPreference()) {
      return;
    }

    if (videoElement.volume === 0) {
      videoElement.volume = 0.5;
    }
    videoElement.muted = false;
    handleAudioPreferenceChange();
  }

  function seekPreview(milliseconds: number) {
    if (!videoElement || !Number.isFinite(milliseconds)) {
      return;
    }

    const nextValue = clampMs(milliseconds);
    playerCurrentMs = nextValue;
    pendingSeekMs = nextValue;

    try {
      videoElement.currentTime = nextValue / 1000;
      pendingSeekMs = null;
    } catch {
      syncVideoMetadata();
    }
  }

  function activeLoopRange() {
    if (!isDraftingMove) {
      return null;
    }

    const startMs = clampMs(isLoopingWithPadding ? draftStartMs : draftActionStartMs);
    const endMs = clampMs(isLoopingWithPadding ? draftEndMs : draftActionEndMs);
    if (endMs <= startMs + 50) {
      return null;
    }

    return { startMs, endMs };
  }

  function enforceLoopAt(milliseconds: number) {
    if (!isLooping || !videoElement) {
      return false;
    }

    const range = activeLoopRange();
    if (!range) {
      isLooping = false;
      return false;
    }

    if (milliseconds >= range.endMs) {
      seekPreview(range.startMs);
      return true;
    }

    return false;
  }

  function syncPlaybackPosition() {
    const currentMs = Math.round((videoElement?.currentTime ?? 0) * 1000);
    if (!enforceLoopAt(currentMs)) {
      playerCurrentMs = currentMs;
    }
  }

  function stopPlaybackAnimation() {
    if (playbackAnimationFrame !== null && browser) {
      cancelAnimationFrame(playbackAnimationFrame);
    }
    playbackAnimationFrame = null;
  }

  function animatePlaybackPosition() {
    playbackAnimationFrame = null;
    syncPlaybackPosition();

    if (videoElement && !videoElement.paused && !videoElement.ended) {
      startPlaybackAnimation();
    }
  }

  function startPlaybackAnimation() {
    if (!browser || playbackAnimationFrame !== null) {
      return;
    }

    playbackAnimationFrame = requestAnimationFrame(animatePlaybackPosition);
  }

  function toggleLoop() {
    if (!isDraftingMove) {
      isLooping = false;
      return;
    }

    isLooping = !isLooping;
    if (!isLooping) {
      return;
    }

    const range = activeLoopRange();
    if (!range) {
      isLooping = false;
    }
  }

  function toggleLoopPadding() {
    isLoopingWithPadding = !isLoopingWithPadding;
    if (!isLooping) {
      return;
    }

    const range = activeLoopRange();
    if (range && (playerCurrentMs < range.startMs || playerCurrentMs > range.endMs)) {
      seekPreview(range.startMs);
    }
  }

  function resetClipPadding() {
    autoClipStart = true;
    autoClipEnd = true;
    clipStartContextMs = DEFAULT_CLIP_HEAD_PADDING_MS;
    clipEndContextMs = DEFAULT_CLIP_TAIL_PADDING_MS;
    draftStartMs = clampClipStartMs(Math.max(0, draftActionStartMs - clipStartContextMs));
    draftEndMs = clampClipEndMs(draftActionEndMs + clipEndContextMs);
  }

  function setDraftBoundary(target: TimelineMarker, valueMs: number, seek = true, snap = true) {
    const nextValue = clampMs(snap ? snapTimelineMoveBoundaryForDrag(target, valueMs) : valueMs);
    lastDraftBoundaryTarget = target;
    if (target === 'playhead') {
      seekPreview(nextValue);
      return;
    }

    if (target === 'clipStart') {
      draftStartMs = clampClipStartMs(nextValue);
      autoClipStart = false;
      clipStartContextMs = Math.max(0, draftActionStartMs - draftStartMs);
      if (seek) seekPreview(draftStartMs);
      return;
    }

    if (target === 'clipEnd') {
      draftEndMs = clampClipEndMs(nextValue);
      autoClipEnd = false;
      clipEndContextMs = Math.max(0, draftEndMs - draftActionEndMs);
      if (seek) seekPreview(draftEndMs);
      return;
    }

    if (target === 'moveStart') {
      draftActionStartMs = Math.min(nextValue, Math.max(0, draftActionEndMs - 250));
      if (autoClipStart) {
        draftStartMs = clampClipStartMs(Math.max(0, draftActionStartMs - Math.max(clipStartContextMs, CLIP_MOVE_BUFFER_MS)));
      } else {
        draftStartMs = clampClipStartMs(draftStartMs);
      }
      syncActiveDraftMoveRowRange();
      if (seek) seekPreview(draftActionStartMs);
      return;
    }

    draftActionEndMs = Math.max(nextValue, draftActionStartMs + 250);
    if (autoClipEnd) {
      draftEndMs = clampClipEndMs(draftActionEndMs + Math.max(clipEndContextMs, CLIP_MOVE_BUFFER_MS));
    } else {
      draftEndMs = clampClipEndMs(draftEndMs);
    }
    syncActiveDraftMoveRowRange();
    if (seek) seekPreview(draftActionEndMs);
  }

  function moveBoundarySnapCandidates(target: TimelineMarker) {
    if (target !== 'moveStart' && target !== 'moveEnd') {
      return [];
    }

    const boundaries = [
      ...draftMoveRows
        .filter((row) => row.id !== activeDraftMoveRowId)
        .flatMap((row) => [row.startMs, row.endMs]),
      ...savedSnapBoundaryClips.flatMap((clip) => [clipActionStartMs(clip), clipActionEndMs(clip)])
    ];
    const minimum = target === 'moveEnd' ? draftActionStartMs + 250 : 0;
    const maximum = target === 'moveStart' ? Math.max(0, draftActionEndMs - 250) : inferredTimelineDurationMs();

    return Array.from(new Set(boundaries.map((value) => Math.round(value))))
      .filter((value) => value >= minimum && value <= maximum)
      .sort((left, right) => left - right);
  }

  function moveBoundarySnapToleranceMs() {
    if (!timelineWidthPx) {
      return 0;
    }

    ensureTimelineViewport();
    const viewportSpanMs = Math.max(0, timelineViewportEndMs - timelineViewportStartMs);
    return Math.ceil((viewportSpanMs / timelineWidthPx) * MOVE_BOUNDARY_SNAP_TOLERANCE_PX);
  }

  function snapTimelineMoveBoundaryForDrag(target: TimelineMarker, valueMs: number) {
    const result = snapMoveBoundaryForDrag({
      target,
      valueMs,
      previousMs: timelineDragPreviousMs,
      snapConsumed: timelineDragSnapConsumed,
      boundaries: moveBoundarySnapCandidates(target),
      toleranceMs: moveBoundarySnapToleranceMs()
    });

    timelineDragPreviousMs = result.previousMs;
    timelineDragSnapConsumed = result.snapConsumed;
    return result.valueMs;
  }

  function timelineMarkerValue(target: TimelineMarker, fallbackMs: number) {
    if (target === 'clipStart') return draftStartMs;
    if (target === 'clipEnd') return draftEndMs;
    if (target === 'moveStart') return draftActionStartMs;
    if (target === 'moveEnd') return draftActionEndMs;
    return fallbackMs;
  }

  function percentForMs(milliseconds: number) {
    ensureTimelineViewport();
    const span = timelineViewportEndMs - timelineViewportStartMs;
    if (!span) {
      return 0;
    }

    return Math.max(0, Math.min(100, ((milliseconds - timelineViewportStartMs) / span) * 100));
  }

  function clipPercentWidth(startMs: number, endMs: number) {
    return Math.max(0, percentForMs(endMs) - percentForMs(startMs));
  }

  function snapPx(px: number) {
    const dpr = browser && typeof window !== 'undefined' ? window.devicePixelRatio || 1 : 1;
    return Math.round(px * dpr) / dpr;
  }

  function syncTimelineWidth() {
    if (!timelineElement) {
      timelineWidthPx = 0;
      return;
    }

    timelineWidthPx = snapPx(timelineElement.getBoundingClientRect().width);
  }

  function timelinePxForMs(milliseconds: number) {
    if (!timelineWidthPx) {
      return 0;
    }

    return snapPx((percentForMs(milliseconds) / 100) * timelineWidthPx);
  }

  function timelineViewportDurationMs() {
    ensureTimelineViewport();
    return Math.max(0, timelineViewportEndMs - timelineViewportStartMs);
  }

  function isTimelineZoomed() {
    if (!isDraftingMove) {
      return false;
    }

    const duration = inferredTimelineDurationMs();
    const span = timelineViewportDurationMs();
    return duration > 0 && span > 0 && span < duration - 1;
  }

  function timelineOverviewLeft(_scaleKey = '') {
    const duration = inferredTimelineDurationMs();
    return duration ? Math.max(0, Math.min(100, (timelineViewportStartMs / duration) * 100)) : 0;
  }

  function timelineOverviewWidth(_scaleKey = '') {
    const duration = inferredTimelineDurationMs();
    return duration ? Math.max(0, Math.min(100, (timelineViewportDurationMs() / duration) * 100)) : 100;
  }

  function timelineRangeStyle(startMs: number, endMs: number, _scaleKey = '') {
    if (!timelineWidthPx) {
      return `left: ${percentForMs(startMs)}%; width: ${clipPercentWidth(startMs, endMs)}%`;
    }

    const leftPx = timelinePxForMs(startMs);
    const rightPx = timelinePxForMs(endMs);
    return `left: ${leftPx}px; width: ${Math.max(0, snapPx(rightPx - leftPx))}px`;
  }

  function markerLeftStyle(milliseconds: number, _scaleKey = '') {
    if (!timelineWidthPx) {
      return `left: ${percentForMs(milliseconds)}%`;
    }

    return `left: ${timelinePxForMs(milliseconds)}px`;
  }

  function draftMoveRangeStyle(row: DraftMoveRow, scaleKey = '') {
    return timelineRangeStyle(row.startMs, row.endMs, scaleKey);
  }

  function savedClipRangeStyle(clip: DerivedClip, scaleKey = '') {
    const startMs = clip.actionStartMs ?? clip.startMs;
    const endMs = clip.actionEndMs ?? clip.endMs;
    return timelineRangeStyle(startMs, endMs, scaleKey);
  }

  function timelineMsFromPointer(event: PointerEvent) {
    if (!timelineElement || !inferredTimelineDurationMs()) {
      return 0;
    }

    ensureTimelineViewport();
    const bounds = timelineElement.getBoundingClientRect();
    const ratio = Math.max(0, Math.min(1, (event.clientX - bounds.left) / bounds.width));
    return Math.round(timelineViewportStartMs + ratio * (timelineViewportEndMs - timelineViewportStartMs));
  }

  function editDraftMoveRowFromTimeline(event: MouseEvent, rowId: string) {
    event.stopPropagation();
    selectDraftMoveRow(rowId);
  }

  function startTimelineDrag(event: PointerEvent, target?: TimelineMarker) {
    if (!inferredTimelineDurationMs() && !syncVideoMetadata()) {
      return;
    }

    event.preventDefault();
    timelineDragCaptureElement = event.currentTarget instanceof HTMLElement ? event.currentTarget : null;
    timelineDragPointerId = event.pointerId;
    timelineDragCaptureElement?.setPointerCapture?.(event.pointerId);
    if (target && target !== 'playhead' && videoElement && !videoElement.paused) {
      resumePlaybackAfterTimelineDrag = true;
      videoElement.pause();
      isPlaying = false;
      stopPlaybackAnimation();
      syncPlaybackPosition();
    } else {
      resumePlaybackAfterTimelineDrag = false;
    }
    timelineDragTarget = target ?? 'playhead';
    timelineDragSnapConsumed = false;
    const nextValue = timelineMsFromPointer(event);
    const markerValue = timelineMarkerValue(timelineDragTarget, nextValue);
    timelineDragPreviousMs = markerValue;
    if (timelineDragTarget === 'playhead') {
      setDraftBoundary(timelineDragTarget, nextValue, true, false);
      timelineDragPreviousMs = nextValue;
      return;
    }

    seekPreview(markerValue);
  }

  function startPlayheadDrag(event: PointerEvent) {
    startTimelineDrag(event, 'playhead');
  }

  function handleTimelinePointerMove(event: PointerEvent) {
    if (!timelineDragTarget) {
      return;
    }

    setDraftBoundary(timelineDragTarget, timelineMsFromPointer(event));
  }

  function stopTimelineDrag() {
    const releasedTarget = timelineDragTarget;
    const shouldResume = resumePlaybackAfterTimelineDrag;
    if (
      timelineDragCaptureElement &&
      timelineDragPointerId !== null &&
      timelineDragCaptureElement.hasPointerCapture?.(timelineDragPointerId)
    ) {
      timelineDragCaptureElement.releasePointerCapture?.(timelineDragPointerId);
    }
    timelineDragCaptureElement = null;
    timelineDragPointerId = null;
    timelineDragTarget = null;
    timelineDragPreviousMs = null;
    timelineDragSnapConsumed = false;
    resumePlaybackAfterTimelineDrag = false;
    if (releasedTarget && releasedTarget !== 'playhead') {
      seekPreview(draftActionStartMs);
      if (shouldResume && videoElement) {
        playbackError = '';
        void videoElement.play().catch((error) => {
          playbackError = error instanceof Error ? error.message : 'Playback could not restart.';
        });
      }
    }
  }

  function handleTimelineWheel(event: WheelEvent) {
    if (!isDraftingMove) {
      resetTimelineZoom();
      return;
    }

    const timelineDurationMs = inferredTimelineDurationMs();
    if (!timelineElement || !timelineDurationMs) {
      return;
    }

    event.preventDefault();
    ensureTimelineViewport();
    const pointerMs = timelineMsFromPointer(event as unknown as PointerEvent);
    const currentSpan = timelineViewportEndMs - timelineViewportStartMs || timelineDurationMs;
    const nextSpan = Math.max(2000, Math.min(timelineDurationMs, currentSpan * (event.deltaY < 0 ? 0.82 : 1.22)));
    const ratio = currentSpan ? (pointerMs - timelineViewportStartMs) / currentSpan : 0.5;
    let nextStart = pointerMs - nextSpan * ratio;
    let nextEnd = nextStart + nextSpan;

    if (nextStart < 0) {
      nextEnd -= nextStart;
      nextStart = 0;
    }

    if (nextEnd > timelineDurationMs) {
      nextStart -= nextEnd - timelineDurationMs;
      nextEnd = timelineDurationMs;
    }

    timelineViewportStartMs = Math.max(0, nextStart);
    timelineViewportEndMs = Math.min(timelineDurationMs, nextEnd);
    hasManualTimelineZoom = timelineViewportStartMs > 0 || timelineViewportEndMs < timelineDurationMs;
  }

  function resetTimelineZoom() {
    timelineViewportStartMs = 0;
    timelineViewportEndMs = inferredTimelineDurationMs();
    hasManualTimelineZoom = false;
  }

  function createDraftClipId() {
    if (typeof globalThis.crypto?.randomUUID === 'function') {
      return globalThis.crypto.randomUUID();
    }

    return `draft-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
  }

  function startNewMoveClip() {
    const actionStart = clampMs(playerCurrentMs || 0);
    const actionEnd = Math.min(
      playerDurationMs || actionStart + DEFAULT_MOVE_DURATION_MS,
      actionStart + DEFAULT_MOVE_DURATION_MS
    );
    enableMoveEditorAudio();
    activeClipId = createDraftClipId();
    isDraftingMove = true;
    draftActionStartMs = actionStart;
    draftActionEndMs = actionEnd;
    const initialRow = createDraftMoveRow(actionStart, actionEnd);
    draftMoveRows = [initialRow];
    activeDraftMoveRowId = initialRow.id;
    draftStartMs = Math.max(0, actionStart - DEFAULT_CLIP_HEAD_PADDING_MS);
    draftEndMs = Math.min(
      playerDurationMs || actionEnd + DEFAULT_CLIP_TAIL_PADDING_MS,
      actionEnd + DEFAULT_CLIP_TAIL_PADDING_MS
    );
    draftStartMs = clampClipStartMs(draftStartMs);
    draftEndMs = clampClipEndMs(draftEndMs);
    autoClipStart = true;
    autoClipEnd = true;
    clipStartContextMs = DEFAULT_CLIP_HEAD_PADDING_MS;
    clipEndContextMs = DEFAULT_CLIP_TAIL_PADDING_MS;
    isLooping = true;
    isLoopingWithPadding = true;
    resetTimelineZoom();
    draftInitialSnapshot = JSON.stringify([
      draftMoveRows.map((row) => [
        row.id,
        row.originalClipId ?? '',
        row.moveIds,
        row.query,
        row.descriptorLabel,
        row.startPositionId ?? '',
        row.endPositionId ?? '',
        row.timingGroupId ?? '',
        row.startMs,
        row.endMs
      ]),
      draftStartMs,
      draftEndMs
    ]);
  }

  function exitDraftMove() {
    activeClipId = null;
    isDraftingMove = false;
    isLooping = false;
    isLoopingWithPadding = true;
    draftMoveRows = [];
    activeDraftMoveRowId = null;
    draftInitialSnapshot = '';
    lastDraftBoundaryTarget = null;
    resumePlaybackAfterTimelineDrag = false;
    renderStatus = '';
  }

  async function togglePlayback() {
    if (!videoElement) {
      return;
    }

    playbackError = '';

    if (videoElement.paused) {
      try {
        await videoElement.play();
      } catch (error) {
        playbackError = error instanceof Error ? error.message : 'Playback could not start.';
      }
      return;
    }

    videoElement.pause();
  }

  function handleVideoTimeUpdate() {
    syncPlaybackPosition();
  }

  function handleKeydown(event: KeyboardEvent) {
    const target = event.target as HTMLElement | null;
    const tagName = target?.tagName?.toLowerCase();
    const isEditableTarget =
      target?.isContentEditable || tagName === 'input' || tagName === 'textarea' || tagName === 'select' || tagName === 'button';

    if ((event.key === ' ' || event.code === 'Space') && selectedAsset && !isEditableTarget) {
      event.preventDefault();
      void togglePlayback();
      return;
    }

    if (event.key === 'Enter' && countMode === 'placing' && !isEditableTarget) {
      event.preventDefault();
      placeCurrentCount();
    }
  }

  function clipRowsWithDraftForSave() {
    const moveRows = draftMoveRows
      .map((row) => ({
        ...row,
        validMoveIds: Array.from(new Set(row.moveIds)).filter((moveId) => moveNameById.has(moveId))
      }))
      .filter((row) => row.validMoveIds.length && row.endMs > row.startMs);
    if (!selectedAsset || !isDraftingMove || !activeClipId || !moveRows.length) {
      renderStatus = 'Choose a valid move and clip range before saving.';
      return null;
    }

    const existingClipIndex = clipRows.findIndex((clip) => clip.id === activeClipId);
    const existingClip = existingClipIndex >= 0 ? clipRows[existingClipIndex] : null;
    const replacedClipIds = new Set(moveRows.map((row) => row.originalClipId).filter((id): id is string => Boolean(id)));
    if (existingClip) {
      replacedClipIds.add(existingClip.id);
    }
    const reusedClipIds = new Set<string>();
    const nextClips = moveRows.flatMap((row) => {
      const clipStartMs = Math.max(0, Math.round(row.startMs - Math.max(clipStartContextMs, CLIP_MOVE_BUFFER_MS)));
      const clipEndMs = Math.max(
        row.endMs + CLIP_MOVE_BUFFER_MS,
        Math.round(row.endMs + Math.max(clipEndContextMs, CLIP_MOVE_BUFFER_MS))
      );

      return row.validMoveIds.map((moveId) => {
        const candidateReusableClip =
          (row.originalClipId ? clipRows.find((clip) => clip.id === row.originalClipId) ?? null : null) ??
          (replacedClipIds.size <= 1 ? existingClip : null);
        const reusableClip =
          candidateReusableClip && !reusedClipIds.has(candidateReusableClip.id) ? candidateReusableClip : null;
        if (reusableClip) {
          reusedClipIds.add(reusableClip.id);
        }

        return {
          id: reusableClip?.id ?? createDraftClipId(),
          sourceAssetId: selectedAsset.id,
          moveId,
          isKeyVideo: reusableClip?.isKeyVideo,
          label: reusableClip?.label ?? null,
          descriptorLabel: row.descriptorLabel.trim() || null,
          startPositionId: row.startPositionId,
          endPositionId: row.endPositionId,
          timingGroupId: row.timingGroupId,
          manuallyNamed: reusableClip?.manuallyNamed ?? false,
          startMs: clipStartMs,
          endMs: clipEndMs,
          actionStartMs: row.startMs,
          actionEndMs: row.endMs,
          cropRect: reusableClip?.cropRect ?? null,
          countMarkers: reusableClip?.countMarkers ?? [],
          countOverlayPlacement: reusableClip?.countOverlayPlacement ?? 'top-left',
          countTimingPreset: reusableClip?.countTimingPreset ?? 'on2-default',
          outputAssetId: reusableClip?.outputAssetId ?? null,
          lowResOutputFilePath: reusableClip?.lowResOutputFilePath ?? null,
          lowResPaddedOutputFilePath: reusableClip?.lowResPaddedOutputFilePath ?? null,
          publishedAssetId: reusableClip?.publishedAssetId ?? null,
          publishedLowResFilePath: reusableClip?.publishedLowResFilePath ?? null,
          publishedLowResPaddedFilePath: reusableClip?.publishedLowResPaddedFilePath ?? null,
          publishedAt: reusableClip?.publishedAt ?? null,
          status: 'pending' as const,
          error: null,
          createdAt: reusableClip?.createdAt ?? new Date().toISOString(),
          updatedAt: new Date().toISOString()
        };
      });
    });

    if (existingClipIndex < 0) {
      return [...clipRows, ...nextClips];
    }

    const nextBaseRows = clipRows.filter((clip) => !replacedClipIds.has(clip.id));
    const insertionIndex = Math.max(0, clipRows.findIndex((clip) => replacedClipIds.has(clip.id)));
    return [...nextBaseRows.slice(0, insertionIndex), ...nextClips, ...nextBaseRows.slice(insertionIndex)];
  }

  async function saveMovesAndQueueRender(continueAdding = false) {
    if (!selectedAsset) {
      return;
    }

    const nextClipRows = clipRowsWithDraftForSave();
    if (!nextClipRows) {
      return;
    }

    renderStatus = 'Saving moves...';
    const response = await fetch('/api/upload/clips', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        sourceAssetId: selectedAsset.id,
        clips: nextClipRows.map((clip) => ({
          id: clip.id,
          moveId: clip.moveId,
          isKeyVideo: clip.isKeyVideo,
          label: clip.label,
          descriptorLabel: clip.descriptorLabel,
          startPositionId: clip.startPositionId,
          endPositionId: clip.endPositionId,
          timingGroupId: clip.timingGroupId,
          manuallyNamed: clip.manuallyNamed,
          startMs: clip.startMs,
          endMs: clip.endMs,
          actionStartMs: clip.actionStartMs,
          actionEndMs: clip.actionEndMs,
          cropRect: clip.cropRect,
          countMarkers: clip.countMarkers,
          countOverlayPlacement: clip.countOverlayPlacement,
          countTimingPreset: clip.countTimingPreset
        }))
      })
    });
    const payload = await response.json();

    if (!response.ok) {
      renderStatus = payload.error ?? 'Could not save moves.';
      return;
    }

    persistedClipRows = payload.clips.map((clip: DerivedClip) => ({ ...clip }));
    clipRows = payload.clips.map((clip: DerivedClip) => ({ ...clip, selected: false }));
    const clipIds = clipRows.map((clip) => clip.id).filter(Boolean);
    activeClipId = null;
    draftMoveRows = [];
    activeDraftMoveRowId = null;
    draftInitialSnapshot = '';

    if (!clipIds.length) {
      renderStatus = 'Moves saved.';
      if (continueAdding) {
        startNewMoveClip();
      } else {
        isDraftingMove = false;
      }
      return;
    }

    renderStatus = 'Queueing renders...';
    const renderResponse = await fetch('/api/upload/render', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ clipIds })
    });
    const renderPayload = await renderResponse.json();

    if (!renderResponse.ok) {
      renderStatus = renderPayload.error ?? 'Moves saved, but render queueing failed.';
      if (continueAdding) {
        startNewMoveClip();
      } else {
        isDraftingMove = false;
      }
      return;
    }

    void renderPayload;
    const queuedIds = new Set(clipIds);
    clipRows = clipRows.map((clip) =>
      queuedIds.has(clip.id) ? { ...clip, status: 'pending', error: null } : clip
    );
    renderStatus = 'Rendering and publishing...';
    startPolling();
    if (continueAdding) {
      startNewMoveClip();
    } else {
      isDraftingMove = false;
    }
  }

  async function saveClipLabelChanges() {
    if (!selectedAsset) {
      return;
    }

    renderStatus = 'Saving clip changes...';
    const response = await fetch('/api/upload/clips', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        sourceAssetId: selectedAsset.id,
        clips: clipRows.map((clip) => ({
          id: clip.id,
          moveId: clip.moveId,
          isKeyVideo: clip.isKeyVideo,
          label: clip.label,
          descriptorLabel: clip.descriptorLabel,
          startPositionId: clip.startPositionId,
          endPositionId: clip.endPositionId,
          timingGroupId: clip.timingGroupId,
          manuallyNamed: clip.manuallyNamed,
          startMs: clip.startMs,
          endMs: clip.endMs,
          actionStartMs: clip.actionStartMs,
          actionEndMs: clip.actionEndMs,
          cropRect: clip.cropRect,
          countMarkers: clip.countMarkers,
          countOverlayPlacement: clip.countOverlayPlacement,
          countTimingPreset: clip.countTimingPreset
        }))
      })
    });
    const payload = await response.json();

    if (!response.ok) {
      renderStatus = payload.error ?? 'Could not save clip changes.';
      return;
    }

    persistedClipRows = payload.clips.map((clip: DerivedClip) => ({ ...clip }));
    clipRows = payload.clips.map((clip: DerivedClip) => ({ ...clip, selected: false }));
    renderStatus = 'Clip changes saved and published.';
    await refreshLibrary(selectedAsset.id);
  }

  async function addMoreMoves() {
    if (!isDraftingMove) {
      startNewMoveClip();
      return;
    }

    const currentMoveRow = draftMoveRows.find((row) => playerCurrentMs >= row.startMs && playerCurrentMs <= row.endMs);
    const shouldStartAfterActiveMove =
      lastDraftBoundaryTarget === 'clipStart' || lastDraftBoundaryTarget === 'clipEnd';
    const actionStart = clampMs(
      currentMoveRow?.endMs ??
        (shouldStartAfterActiveMove ? activeDraftMoveRow?.endMs : null) ??
        (Number.isFinite(playerCurrentMs) ? playerCurrentMs : draftActionEndMs || 0)
    );
    const actionEnd = Math.min(
      playerDurationMs || actionStart + DEFAULT_MOVE_DURATION_MS,
      actionStart + DEFAULT_MOVE_DURATION_MS
    );
    const nextRow = createDraftMoveRow(actionStart, actionEnd);
    draftMoveRows = [...draftMoveRows, nextRow];
    selectDraftMoveRow(nextRow.id);
  }

  function addBoundMove(rowId = activeDraftMoveRowId) {
    const sourceRow = draftMoveRows.find((row) => row.id === rowId) ?? activeDraftMoveRow;
    if (!sourceRow) {
      return;
    }

    const timingGroupId = sourceRow.timingGroupId ?? createUiId('clip-group');
    const nextRow = createDraftMoveRow(sourceRow.startMs, sourceRow.endMs, timingGroupId);
    draftMoveRows = draftMoveRows.flatMap((row) =>
      row.id === sourceRow.id
        ? [
            {
              ...row,
              timingGroupId
            },
            nextRow
          ]
        : [row]
    );
    selectDraftMoveRow(nextRow.id);
  }

  function applyRenderStatuses(
    statuses: Array<{ clipId: string; status: DerivedClip['status']; error: string | null; outputAssetId: string | null }>
  ) {
    const byId = new Map(statuses.map((entry) => [entry.clipId, entry]));
    clipRows = clipRows.map((clip) => {
      const status = byId.get(clip.id);
      if (!status) {
        return clip;
      }

      return {
        ...clip,
        status: status.status,
        error: status.error,
        outputAssetId: status.outputAssetId
      };
    });
  }

  function stopPolling() {
    if (pollTimer) {
      clearTimeout(pollTimer);
      pollTimer = null;
    }
  }

  async function pollRenderStatuses() {
    const clipIds = clipRows
      .filter((clip) => clip.status === 'pending' || clip.status === 'rendering')
      .map((clip) => clip.id);

    if (!clipIds.length) {
      stopPolling();
      await refreshLibrary(selectedAssetId);
      return;
    }

    const response = await fetch(`/api/upload/render?ids=${encodeURIComponent(clipIds.join(','))}`);
    if (!response.ok) {
      pollTimer = setTimeout(() => void pollRenderStatuses(), 2500);
      return;
    }

    const payload = await response.json();
    applyRenderStatuses(payload.statuses);

    const stillPending = clipRows.some((clip) => clip.status === 'pending' || clip.status === 'rendering');
    if (stillPending) {
      pollTimer = setTimeout(() => void pollRenderStatuses(), 2500);
      return;
    }

    renderStatus = 'Render complete and published to moves.';
    stopPolling();
    await refreshLibrary(selectedAssetId);
  }

  function startPolling() {
    if (!browser) {
      return;
    }

    stopPolling();
    pollTimer = setTimeout(() => void pollRenderStatuses(), 1200);
  }

  onDestroy(() => {
    timelineResizeObserver?.disconnect();
    stopPolling();
    stopPlaybackAnimation();
  });
</script>

<svelte:head>
  <title>Media | Salsa Encyclopedia</title>
</svelte:head>

<svelte:window on:pointermove={handleTimelinePointerMove} on:pointerup={stopTimelineDrag} on:keydown={handleKeydown} />

<div class="stack upload-page media-page media-editor-page">
  <section class="panel upload-shell">
    <div class="panel-header upload-header">
      <a class="pill move-backlink" href="/media">Back to media</a>
    </div>

    <div class="upload-grid">
      <div class="upload-column">
        <div
          class={`meta-card upload-card media-add-card ${isUploadingDragOver ? 'drag-over' : ''}`}
          on:dragover={(event) => {
            event.preventDefault();
            isUploadingDragOver = true;
          }}
          on:dragleave={() => (isUploadingDragOver = false)}
          on:drop={handleUploadDrop}
        >
          <div class="panel-header">
            <h3>Add new</h3>
          </div>
          <div class="upload-form">
            <label class="media-file-target">
              <span>{uploadFile ? `Uploading ${uploadFile.name}` : 'Drop a video here or click to choose'}</span>
              <input
                type="file"
                accept="video/*"
                on:change={(event) => void uploadSourceFile((event.currentTarget as HTMLInputElement).files?.[0] ?? null)}
              />
            </label>

            {#if uploadStatus}
              <div class="upload-actions">
                <span class="muted">{uploadStatus}</span>
              </div>
            {/if}
          </div>
        </div>

        <div class="meta-card upload-card media-library-card">
          <div class="panel-header">
            <div class="panel-heading-row">
              <h3>Uploaded clips</h3>
              <span class="muted">{assets.length} / {totalAssets} videos</span>
            </div>
            {#if deleteStatus}
              <p class="muted">{deleteStatus}</p>
            {/if}
          </div>
          <div class="source-library-list media-card-list" bind:this={mediaListElement} on:scroll={handleMediaListScroll}>
            {#if assets.length}
              {#each mediaGroups as group}
                <h4 class="media-month-heading">{group.month}</h4>
                {#each group.assets as asset}
                  <div class:selected={selectedAssetId === asset.id} class="source-library-row media-source-card">
                    <button
                      type="button"
                      class="source-library-item media-source-button"
                      on:click={() => selectAsset(asset.id)}
                    >
                      <span class="media-card-poster">
                        {#if asset.posterFile}
                          <img src={posterUrl(asset.posterFile)} alt="" loading="lazy" />
                        {:else}
                          <span class="media-card-poster-empty">No preview</span>
                        {/if}
                      </span>
                      <span class="media-card-copy">
                        <strong>{asset.displayName}</strong>
                        <span>{videoMetaLabel(asset)}</span>
                        <span>Uploaded {formatDate(asset.createdAt)} · {asset.clips.length} clips</span>
                        {#if asset.recordDate}<span>Recorded {formatDate(asset.recordDate)}</span>{/if}
                        {#if asset.classWorkshop}<span>{asset.classWorkshop}</span>{/if}
                        {#if asset.tags.length}<span>{asset.tags.join(', ')}</span>{/if}
                      </span>
                    </button>
                    <button
                      class="source-library-delete"
                      type="button"
                      aria-label={`Delete ${asset.displayName}`}
                      on:click={() => void deleteSourceVideo(asset)}
                    >
                      Delete
                    </button>
                  </div>
                {/each}
              {/each}
              {#if nextCursor}
                <button class="media-load-more" type="button" disabled={isLoadingMore} on:click={() => void loadMoreMedia()}>
                  {isLoadingMore ? 'Loading...' : 'Load more'}
                </button>
              {/if}
            {:else}
              <p class="muted">No uploaded source videos yet.</p>
            {/if}
          </div>
        </div>
      </div>

      <div class="upload-column upload-column-wide">
        {#if selectedAsset}
          <div class={`media-editor-default-layout ${isDraftingMove ? 'editor-active' : ''}`}>
            <div class="video-panel upload-card upload-editor-shell">
              <div class="editor-stage">
                <div
                  class="video-frame upload-preview-frame"
                  class:crop-active={isCroppingClip}
                  role="application"
                  aria-label="Video crop area"
                  on:pointerdown={startCropDrag}
                  on:pointermove={updateCropDrag}
                  on:pointerup={finishCropDrag}
                >
                  <video
                    bind:this={videoElement}
                    playsinline
                    preload="auto"
                    src={mediaUrl(selectedAsset.filePath)}
                    on:loadedmetadata={() => {
                      syncVideoMetadata();
                      applyPendingSeek();
                    }}
                    on:durationchange={() => {
                      syncVideoMetadata();
                      applyPendingSeek();
                    }}
                    on:loadeddata={() => {
                      syncVideoMetadata();
                      applyPendingSeek();
                    }}
                    on:canplay={() => {
                      syncVideoMetadata();
                      applyPendingSeek();
                      void initializeSelectedVideo();
                    }}
                    on:timeupdate={handleVideoTimeUpdate}
                    on:play={() => {
                      isPlaying = true;
                      startPlaybackAnimation();
                    }}
                    on:pause={() => {
                      isPlaying = false;
                      stopPlaybackAnimation();
                      syncPlaybackPosition();
                    }}
                    on:ended={() => {
                      isPlaying = false;
                      stopPlaybackAnimation();
                    }}
                    on:volumechange={handleAudioPreferenceChange}
                    on:error={() => (playbackError = 'This browser could not load the selected video.')}
                    on:click={() => void togglePlayback()}
                  ></video>
                  {#if activeSavedClip?.cropRect}
                    <span class="clip-crop-box" style={cropRectStyle(activeSavedClip.cropRect)}></span>
                  {/if}
                  {#if activePreviewCountMarker && activeSavedClip}
                    <span class={countOverlayClass(activeSavedClip.countOverlayPlacement)}>
                      {activePreviewCountMarker.count}
                    </span>
                  {/if}
                  {#if playbackError}
                    <p class="editor-playback-error">{playbackError}</p>
                  {/if}
                </div>
              </div>

              <div class:expanded={isDraftingMove} class="timeline-card">
                <div class="timeline-meta">
                  {#if isDraftingMove}
                    <span><strong>Clip</strong> {formatSeconds(draftStartMs)}s - {formatSeconds(draftEndMs)}s</span>
                    <span><strong>Move</strong> {formatSeconds(draftActionStartMs)}s - {formatSeconds(draftActionEndMs)}s</span>
                    <span><strong>Length</strong> {formatSeconds(Math.max(0, draftEndMs - draftStartMs))}s</span>
                  {/if}
                  <span class="editor-video-controls" on:click={(event) => event.stopPropagation()}>
                    {#if !timelineDragTarget}
                      <button
                        class="editor-video-play"
                        class:playing={isPlaying}
                        type="button"
                        aria-label={isPlaying ? 'Pause source video' : 'Play source video'}
                        on:click={() => void togglePlayback()}
                      >
                        <span>{isPlaying ? 'Pause' : 'Play'}</span>
                      </button>
                    {/if}
                    <span class="editor-video-volume-wrap" class:open={isVolumeOpen}>
                      <button
                        class="editor-video-volume-toggle"
                        class:muted={isMuted || videoVolume === 0}
                        type="button"
                        aria-label="Adjust source video volume"
                        aria-expanded={isVolumeOpen}
                        on:click={toggleVolumeOpen}
                      >
                        <span>Volume</span>
                      </button>
                      <span class="editor-video-volume-panel">
                        <input
                          class="editor-video-volume"
                          type="range"
                          min="0"
                          max="1"
                          step="0.05"
                          value={videoVolume}
                          aria-label="Source video volume"
                          on:input={handleVolumeInput}
                        />
                      </span>
                    </span>
                  </span>
                  <span><strong>Total</strong> {formatRoundedSeconds(playerDurationMs)}s</span>
                  {#if isDraftingMove}
                    <span class:timeline-zoom-active={isTimelineZoomed()}>
                      <strong>{isTimelineZoomed() ? 'Zoomed' : 'Full view'}</strong>
                      {formatRoundedSeconds(timelineViewportDurationMs())}s
                      {#if isTimelineZoomed()}
                        ({formatRoundedSeconds(timelineViewportStartMs)}s - {formatRoundedSeconds(timelineViewportEndMs)}s)
                      {/if}
                    </span>
                    <span class="timeline-tool-actions">
                      <button
                        class:active={isLooping}
                        class="timeline-loop-button"
                        type="button"
                        aria-pressed={isLooping}
                        aria-label="Loop clip range"
                        title="Loop clip range"
                        on:click={toggleLoop}
                      >
                        ⟳ loop
                      </button>
                      <button
                        class:active={isLoopingWithPadding}
                        class="timeline-loop-button"
                        type="button"
                        aria-pressed={isLoopingWithPadding}
                        aria-label="Loop with padding"
                        title="Loop with padding"
                        on:click={toggleLoopPadding}
                      >
                        With padding
                      </button>
                      <button
                        class="timeline-loop-button"
                        type="button"
                        aria-label="Reset clip padding"
                        title="Reset clip padding"
                        on:click={resetClipPadding}
                      >
                        Reset padding
                      </button>
                      {#if isTimelineZoomed()}
                        <button
                          class="timeline-loop-button"
                          type="button"
                          aria-label="Reset timeline zoom"
                          title="Reset timeline zoom"
                          on:click={resetTimelineZoom}
                        >
                          Reset zoom
                        </button>
                      {/if}
                    </span>
                  {/if}
                </div>
                {#if !isDraftingMove && playbackMoveClips.length}
                  <div class="timeline-now-playing" aria-label="Current move context">
                    <div class="timeline-context-slot timeline-context-slot-previous">
                      {#if visiblePreviousPlaybackMove}
                        <div class="timeline-context-box timeline-context-side timeline-context-previous">
                          <strong>{playbackMoveName(visiblePreviousPlaybackMove)}</strong>
                        </div>
                      {/if}
                    </div>
                    <div class="timeline-context-box timeline-context-current" class:empty={!currentPlaybackMove}>
                      <strong>{currentPlaybackMove ? playbackMoveName(currentPlaybackMove) : '—'}</strong>
                    </div>
                    <div class="timeline-context-slot timeline-context-slot-next">
                      {#if visibleNextPlaybackMove}
                        <div class="timeline-context-box timeline-context-side timeline-context-next">
                          <strong>{playbackMoveName(visibleNextPlaybackMove)}</strong>
                        </div>
                      {/if}
                    </div>
                  </div>
                {/if}
                {#if isTimelineZoomed()}
                  <div class="timeline-overview zoomed" aria-hidden="true">
                    <span class="timeline-overview-track">
                      <span
                        class="timeline-overview-window"
                        style={`left: ${timelineOverviewLeft(timelineScaleKey)}%; width: ${timelineOverviewWidth(timelineScaleKey)}%`}
                      ></span>
                    </span>
                  </div>
                {/if}
                <div
                  class:zoomed={isTimelineZoomed()}
                  class="clip-timeline"
                  class:inactive={!isDraftingMove}
                  bind:this={timelineElement}
                  role="slider"
                  tabindex="0"
                  aria-label="Clip range timeline"
                  aria-valuemin="0"
                  aria-valuemax={playerDurationMs}
                  aria-valuenow={playerCurrentMs}
                  on:pointerdown={(event) => startTimelineDrag(event)}
                  on:wheel={handleTimelineWheel}
                >
                  <div class="clip-timeline-track"></div>
                  {#each visibleSavedTimelineClips as clip (clip.id)}
                    <button
                      type="button"
                      class="clip-timeline-selection saved-move-range"
                      class:active={activeClipId === clip.id}
                      class:changed={Boolean(clipChangeStates.get(clip.id))}
                      style={savedClipRangeStyle(clip, timelineScaleKey)}
                      aria-label={`${clipDisplayName(clip)} ${clipChangeLabel(clipChangeStates.get(clip.id)).toLowerCase()} clip`}
                      title={`${clipDisplayName(clip)} · ${clipChangeLabel(clipChangeStates.get(clip.id))}`}
                      on:pointerdown={(event) => event.stopPropagation()}
                      on:click={(event) => {
                        event.stopPropagation();
                        openSavedClipEditor(clip);
                      }}
                    ></button>
                  {/each}
                  {#if isDraftingMove}
                    <div
                      class="clip-timeline-selection clip-range"
                      style={timelineRangeStyle(draftStartMs, draftEndMs, timelineScaleKey)}
                    ></div>
                    {#each draftMoveRows as row (row.id)}
                      <div
                        class="clip-timeline-selection move-range"
                        class:active={row.id === activeDraftMoveRowId}
                        class:changed={hasSaveableDraftChanges}
                        class:secondary={row.id !== activeDraftMoveRowId}
                        style={draftMoveRangeStyle(row, timelineScaleKey)}
                        on:dblclick={(event) => editDraftMoveRowFromTimeline(event, row.id)}
                      ></div>
                    {/each}
                  {/if}
                  <div class="clip-timeline-playhead" style={markerLeftStyle(playerCurrentMs, timelineScaleKey)}></div>
                  <div
                    role="button"
                    tabindex={playerDurationMs ? 0 : -1}
                    class="clip-timeline-playhead-handle"
                    style={markerLeftStyle(playerCurrentMs, timelineScaleKey)}
                    aria-label="Drag playback position"
                    title="Playback position"
                    on:pointerdown={(event) => (event.stopPropagation(), startPlayheadDrag(event))}
                  ></div>
                  {#if isDraftingMove}
                  <button
                    type="button"
                    class="clip-timeline-marker clip-marker"
                    style={markerLeftStyle(draftStartMs, timelineScaleKey)}
                    aria-label="Drag clip start"
                    title="Clip starts"
                    on:pointerdown={(event) => (event.stopPropagation(), startTimelineDrag(event, 'clipStart'))}
                  >
                    <span class="clip-timeline-marker-head" aria-hidden="true">
                      <span class="clip-timeline-marker-fill"></span>
                    </span>
                    <span class="clip-timeline-marker-stem" aria-hidden="true"></span>
                  </button>
                  <button
                    type="button"
                    class="clip-timeline-marker clip-marker"
                    style={markerLeftStyle(draftEndMs, timelineScaleKey)}
                    aria-label="Drag clip end"
                    title="Clip ends"
                    on:pointerdown={(event) => (event.stopPropagation(), startTimelineDrag(event, 'clipEnd'))}
                  >
                    <span class="clip-timeline-marker-head" aria-hidden="true">
                      <span class="clip-timeline-marker-fill"></span>
                    </span>
                    <span class="clip-timeline-marker-stem" aria-hidden="true"></span>
                  </button>
                  <button
                    type="button"
                    class="clip-timeline-marker move-marker"
                    style={markerLeftStyle(draftActionStartMs, timelineScaleKey)}
                    aria-label="Drag move start"
                    title="Move starts"
                    on:pointerdown={(event) => (event.stopPropagation(), startTimelineDrag(event, 'moveStart'))}
                  >
                    <span class="clip-timeline-marker-head" aria-hidden="true">
                      <span class="clip-timeline-marker-fill"></span>
                    </span>
                    <span class="clip-timeline-marker-stem" aria-hidden="true"></span>
                  </button>
                  <button
                    type="button"
                    class="clip-timeline-marker move-marker"
                    style={markerLeftStyle(draftActionEndMs, timelineScaleKey)}
                    aria-label="Drag move end"
                    title="Move ends"
                    on:pointerdown={(event) => (event.stopPropagation(), startTimelineDrag(event, 'moveEnd'))}
                  >
                    <span class="clip-timeline-marker-head" aria-hidden="true">
                      <span class="clip-timeline-marker-fill"></span>
                    </span>
                    <span class="clip-timeline-marker-stem" aria-hidden="true"></span>
                  </button>
                  {/if}
                </div>
                <div class="timeline-move-actions">
                  {#if isDraftingMove}
                    <button class="timeline-move-action" type="button" on:click={exitDraftMove}>Exit</button>
                    <button class="timeline-move-action" type="button" on:click={() => void addMoreMoves()}>Add move</button>
                    {#if activeDraftMoveRow}
                      <button class="timeline-move-action" type="button" on:click={() => addBoundMove()}>Add bound move</button>
                    {/if}
                    {#if hasSaveableDraftChanges}
                      <button class="timeline-move-action primary" type="button" on:click={() => saveMovesAndQueueRender()}>
                        Save
                      </button>
                    {/if}
                  {:else}
                    <button class="timeline-move-action" type="button" on:click={addMoreMoves}>Edit moves</button>
                  {/if}
                  {#if activeSavedClip}
                    <button
                      class="timeline-move-action key-video-star"
                      type="button"
                      aria-pressed={activeSavedClip.isKeyVideo}
                      title={activeSavedClip.isKeyVideo ? 'Remove key video' : 'Make key video'}
                      on:click={() => toggleClipKeyVideo(activeSavedClip.id)}
                    >
                      {activeSavedClip.isKeyVideo ? '★' : '☆'}
                    </button>
                  {/if}
                  {#if hasUnsavedClipRowChanges}
                    <button class="timeline-move-action primary" type="button" on:click={() => void saveClipLabelChanges()}>
                      Save clip changes
                    </button>
                  {/if}
                </div>
                {#if isDraftingMove}
                  <div class="clip-draft-form editor-draft-form">
                    <div class="draft-move-header" aria-hidden="true">
                      <span>Start</span>
                      <span>Move</span>
                      <span>Label</span>
                      <span>Start pos</span>
                      <span>End pos</span>
                      <span></span>
                      <span></span>
                      <span></span>
                    </div>
                    {#each editorMoveRows as item (item.key)}
                      {#if item.kind === 'draft'}
                        {@const row = item.row}
                        <div class="draft-move-row" class:active={row.id === activeDraftMoveRowId} class:bound={Boolean(row.timingGroupId)}>
                          <div class="move-start-display">
                            <strong>{formatTenthSeconds(row.startMs)}s</strong>
                          </div>
                          <div class="move-link-field">
                            <MovePicker
                              moves={availableMoves}
                              selectedIds={row.moveIds}
                              excludedIds={selectedDraftMoveIds}
                              query={row.query}
                              limit={MOVE_SUGGESTION_LIMIT}
                              selectedPlacement="inside"
                              allowCreate={true}
                              maxSelected={1}
                              on:focus={() => selectDraftMoveRow(row.id)}
                              on:query={(event) => handleDraftMoveQueryInput(row.id, event.detail.query)}
                              on:create={(event) => createDraftMoveFromQuery(row.id, event.detail.query)}
                              on:select={(event) => addDraftMove(event.detail.moveId, row.id)}
                              on:remove={(event) => removeDraftMove(row.id, event.detail.moveId)}
                            />
                          </div>
                          <label class="draft-descriptor-field">
                            <span class="sr-only">Extra label</span>
                            <input
                              value={row.descriptorLabel}
                              aria-label="Extra move label"
                              placeholder="Extra label"
                              on:focus={() => selectDraftMoveRow(row.id)}
                              on:input={(event) => updateDraftDescriptor(row.id, event.currentTarget.value)}
                            />
                          </label>
                          <div class="draft-position-field">
                            <SearchablePicker
                              options={positionPickerOptions}
                              selectedIds={row.startPositionId ? [row.startPositionId] : []}
                              query={row.startPositionQuery}
                              limit={MOVE_SUGGESTION_LIMIT}
                              placeholder="Start"
                              addPlaceholder="Start"
                              ariaLabel="Start position"
                              selectedPlacement="inside"
                              floatingDropdown={true}
                              on:focus={() => selectDraftMoveRow(row.id)}
                              on:query={(event) => updateDraftPositionQuery(row.id, 'start', event.detail.query)}
                              on:select={(event) => selectDraftPosition(row.id, 'start', event.detail.id)}
                              on:remove={() => removeDraftPosition(row.id, 'start')}
                            />
                          </div>
                          <div class="draft-position-field">
                            <SearchablePicker
                              options={positionPickerOptions}
                              selectedIds={row.endPositionId ? [row.endPositionId] : []}
                              query={row.endPositionQuery}
                              limit={MOVE_SUGGESTION_LIMIT}
                              placeholder="End"
                              addPlaceholder="End"
                              ariaLabel="End position"
                              selectedPlacement="inside"
                              floatingDropdown={true}
                              on:focus={() => selectDraftMoveRow(row.id)}
                              on:query={(event) => updateDraftPositionQuery(row.id, 'end', event.detail.query)}
                              on:select={(event) => selectDraftPosition(row.id, 'end', event.detail.id)}
                              on:remove={() => removeDraftPosition(row.id, 'end')}
                            />
                          </div>
                          {#if row.id === activeDraftMoveRowId}
                            <button class="draft-edit-button active" type="button" disabled aria-pressed="true">Editing</button>
                          {:else}
                            <button class="draft-edit-button" type="button" on:click={() => selectDraftMoveRow(row.id)}>Edit</button>
                          {/if}
                        </div>
                      {:else}
                        {@const clip = item.clip}
                        <div class="draft-move-row saved-editor-row" class:bound={Boolean(clip.timingGroupId)}>
                          <button
                            class="move-start-display saved-editor-start"
                            type="button"
                            on:click={() => openSavedClipEditor(clip)}
                          >
                            <strong>{formatTenthSeconds(clipActionStartMs(clip))}s</strong>
                          </button>
                          <button
                            class="move-link-field saved-editor-move"
                            type="button"
                            on:click={() => openSavedClipEditor(clip)}
                          >
                            <span class="move-chip move-picker-inline-chip saved-editor-chip">
                              {clip.moveDisplayId ?? clip.moveId} · {moveNameById.get(clip.moveId) ?? clip.moveDisplayId ?? clip.moveId}
                            </span>
                          </button>
                          <button
                            class="saved-editor-cell"
                            class:empty={!clip.descriptorLabel}
                            type="button"
                            on:click={() => openSavedClipEditor(clip)}
                          >
                            {clip.descriptorLabel || '—'}
                          </button>
                          <button
                            class="saved-editor-cell"
                            class:empty={!clip.startPositionId}
                            type="button"
                            on:click={() => openSavedClipEditor(clip)}
                          >
                            {positionLabel(clip.startPositionId) || '—'}
                          </button>
                          <button
                            class="saved-editor-cell"
                            class:empty={!clip.endPositionId}
                            type="button"
                            on:click={() => openSavedClipEditor(clip)}
                          >
                            {positionLabel(clip.endPositionId) || '—'}
                          </button>
                          <button
                            class="draft-edit-button key-video-star"
                            type="button"
                            aria-pressed={clip.isKeyVideo}
                            title={clip.isKeyVideo ? 'Remove key video' : 'Make key video'}
                            on:click={() => toggleClipKeyVideo(clip.id)}
                          >
                            {clip.isKeyVideo ? '★' : '☆'}
                          </button>
                          <button class="draft-edit-button" type="button" on:click={() => openSavedClipEditor(clip)}>
                            Edit
                          </button>
                          <button class="draft-edit-button danger" type="button" on:click={() => removeSavedClip(clip.id)}>
                            Delete
                          </button>
                        </div>
                      {/if}
                    {/each}
                  </div>
                {/if}
                {#if renderStatus}
                  <p class="timeline-render-status muted">{renderStatus}</p>
                {/if}
              </div>
            </div>

            <div class="meta-card upload-card media-properties-card">
              <div class="panel-header">
                <div class="panel-heading-row">
                  <div class="media-properties-title">
                    <h3>{selectedAsset.displayName}</h3>
                  </div>
                  <button class="icon-button property-edit-button" type="button" aria-label="Edit properties" title="Edit properties" on:click={() => (isEditingMetadata = !isEditingMetadata)}>✎</button>
                </div>
              </div>
              {#if isEditingMetadata}
                <div class="upload-form compact-property-form">
                  <label>
                    <span>Name</span>
                    <input bind:value={editDisplayName} />
                  </label>
                  <label>
                    <span>Dancers</span>
                    <SearchablePicker
                      options={dancerOptions}
                      selectedIds={editDancerIds}
                      query={editDancerQuery}
                      placeholder="Search dancers"
                      addPlaceholder="Add dancer"
                      ariaLabel="Dancers"
                      selectedPlacement="inside"
                      floatingDropdown={true}
                      on:query={(event) => (editDancerQuery = event.detail.query)}
                      on:select={(event) => addSelectedDancer(event.detail.option.label)}
                      on:remove={(event) => removeSelectedDancer(event.detail.id)}
                    />
                  </label>
                  <div class="segmented-field">
                    <span class="segmented-label source-segmented-label">Source</span>
                    <div class="segmented-control" role="radiogroup" aria-label="Source">
                      <button
                        type="button"
                        class:active={editOriginType === 'self-recorded'}
                        aria-pressed={editOriginType === 'self-recorded'}
                        on:click={() => (editOriginType = 'self-recorded')}
                      >
                        Self-recorded
                      </button>
                      <button
                        type="button"
                        class:active={editOriginType === 'download'}
                        aria-pressed={editOriginType === 'download'}
                        on:click={() => (editOriginType = 'download')}
                      >
                        Download
                      </button>
                    </div>
                  </div>
                  {#if editOriginType === 'download'}
                    <label>
                      <span>Source URL</span>
                      <input type="url" bind:value={editSourceUrl} placeholder="https://..." />
                    </label>
                  {/if}
                  <div class="segmented-field">
                    <span class="segmented-label">Timing</span>
                    <div class="segmented-control" role="radiogroup" aria-label="Timing">
                      {#each data.timingOptions as timing}
                        <button
                          type="button"
                          class={timingBadgeClass(timing.value)}
                          class:active={editTiming === timing.value}
                          aria-pressed={editTiming === timing.value}
                          on:click={() => (editTiming = timing.value)}
                        >
                          {timing.label}
                        </button>
                      {/each}
                    </div>
                  </div>
                  <div class="segmented-field">
                    <span class="segmented-label">Type</span>
                    <div class="segmented-control" role="radiogroup" aria-label="Type">
                      {#each data.contentTypeOptions as contentType}
                        <button
                          type="button"
                          class={contentBadgeClass(contentType.value)}
                          class:active={editContentType === contentType.value}
                          aria-pressed={editContentType === contentType.value}
                          on:click={() => (editContentType = contentType.value)}
                        >
                          {contentType.label}
                        </button>
                      {/each}
                    </div>
                  </div>
                  <div class="segmented-field">
                    <span class="segmented-label">Environment</span>
                    <div class="segmented-control" role="radiogroup" aria-label="Environment">
                      {#each data.environmentOptions as environment}
                        <button
                          type="button"
                          class={environmentBadgeClass(environment.value)}
                          class:active={editEnvironment === environment.value}
                          aria-pressed={editEnvironment === environment.value}
                          on:click={() => (editEnvironment = environment.value)}
                        >
                          {environment.label}
                        </button>
                      {/each}
                    </div>
                  </div>
                  <label>
                    <span>Upload date</span>
                    <input type="date" bind:value={editCreatedAt} />
                  </label>
                  <label>
                    <span>Record date</span>
                    <input type="date" bind:value={editRecordDate} />
                  </label>
                  <label>
                    <span>Other tags</span>
                    <input
                      list="media-tag-options"
                      bind:value={editTagDraft}
                      on:keydown={handleTagDraftKeydown}
                    />
                  </label>
                  {#if editTags.length}
                    <div class="tag-chip-row">
                      {#each editTags as tag}
                        <button type="button" class="tag-chip" on:click={() => removeTag(tag)}>{tag} ×</button>
                      {/each}
                    </div>
                  {/if}
                  <label>
                    <span>Notes</span>
                    <textarea bind:value={editNotes} rows="3"></textarea>
                  </label>

                  <div class="upload-actions">
                    <button type="button" on:click={detectSourceFields}>Detect fields</button>
                    <button class="primary" type="button" on:click={saveSourceMetadata}>Save</button>
                    <button type="button" on:click={() => (syncingAssetKey = null)}>Cancel</button>
                    {#if detectStatus || saveStatus}
                      <span class="muted">{detectStatus || saveStatus}</span>
                    {/if}
                  </div>
                </div>
              {:else}
                <dl class="property-list">
                  <dt class="sr-only">Source attributes</dt>
                  <dd class="media-property-badges">
                    <ContentBadge label={timingLabel(selectedAsset.timing)} className={`media-property-badge ${timingBadgeClass(selectedAsset.timing)}`} />
                    <ContentBadge label={contentTypeLabel(selectedAsset.contentType)} className={`media-property-badge ${contentBadgeClass(selectedAsset.contentType)}`} />
                    <ContentBadge label={environmentLabel(selectedAsset.environment)} className={`media-property-badge ${environmentBadgeClass(selectedAsset.environment)}`} />
                  </dd>
                  <dt>Dancers</dt>
                  <dd>
                    {#if selectedAsset.dancers.length}
                      <span class="shared-chip-row media-dancer-badge-row">
                        {#each selectedAsset.dancers as dancer}
                          <span class="shared-chip media-dancer-badge">{dancer}</span>
                        {/each}
                      </span>
                    {:else}
                      —
                    {/if}
                  </dd>
                  <dt>Recorded</dt>
                  <dd>{selectedAsset.recordDate ? formatDate(selectedAsset.recordDate) : '—'}</dd>
                  <dt>Uploaded</dt>
                  <dd>{selectedAsset.createdAt ? formatDate(selectedAsset.createdAt) : '—'}</dd>
                  <dt>Source</dt>
                  <dd>{selectedAsset.originType === 'download' ? 'Download' : 'Self-recorded'}</dd>
                  {#if selectedAsset.originType === 'download' && selectedAsset.sourceUrl}
                    <dt>Source URL</dt>
                    <dd><a href={selectedAsset.sourceUrl} target="_blank" rel="noreferrer">{sourceUrlLabel(selectedAsset.sourceUrl)}</a></dd>
                  {/if}
                  {#if selectedAsset.tags.length}
                    <dt>Tags</dt>
                    <dd>{selectedAsset.tags.join(', ')}</dd>
                  {/if}
                  {#if selectedAsset.notes}
                    <dt>Notes</dt>
                    <dd>{selectedAsset.notes}</dd>
                  {/if}
                </dl>
              {/if}
            </div>
          </div>

        {:else}
          <div class="meta-card upload-card">
            <p class="muted">Upload a source video to start building shared move clips.</p>
          </div>
        {/if}
      </div>
    </div>
  </section>

  <datalist id="media-tag-options">
    {#each suggestions.tags as tag}
      <option value={tag}></option>
    {/each}
  </datalist>
</div>
