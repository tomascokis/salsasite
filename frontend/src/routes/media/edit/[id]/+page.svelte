<script lang="ts">
  import { browser } from '$app/environment';
  import { onDestroy, tick } from 'svelte';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import { publicationStatusFor, processingStatusFor } from '$lib/content-status';
  import type {
    ClipCountMarker,
    ClipCropRect,
    CountOverlayPlacement,
    CountTimingPreset,
    DerivedClip,
    VideoContentType,
    VideoEnvironment,
    VideoOriginType,
    VideoTiming
  } from '$lib/types';

  type MoveOption = {
    id: string;
    slug: string;
    name: string | null;
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
    moveIds: string[];
    query: string;
    startMs: number;
    endMs: number;
  };
  type TimelineMarker = 'clipStart' | 'clipEnd' | 'moveStart' | 'moveEnd' | 'playhead';
  type CountModeStep = 'idle' | 'placing';
  const MOVE_SUGGESTION_LIMIT = 8;
  const CLIP_MOVE_BUFFER_MS = 500;
  const DEFAULT_CLIP_PADDING_MS = 5000;
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
  let publishStatus = '';
  let deleteStatus = '';
  let uploadFile: File | null = null;
  let editDisplayName = '';
  let editDancers = '';
  let editTiming: VideoTiming = 'on2';
  let editContentType: VideoContentType = 'music';
  let editEnvironment: VideoEnvironment = 'class';
  let editOriginType: VideoOriginType = 'self-recorded';
  let editSourceUrl = '';
  let editCreatedAt = '';
  let editRecordDate = '';
  let editClassWorkshop = '';
  let editTags: string[] = [];
  let editTagDraft = '';
  let editNotes = '';
  let isEditingMetadata = false;
  let isUploadingDragOver = false;
  let isLoadingMore = false;
  let clipRows: ClipWithUi[] = [];
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
  let clipStartContextMs = 5000;
  let clipEndContextMs = 5000;
  let lastDraftBoundaryTarget: TimelineMarker | null = null;
  let videoElement: HTMLVideoElement | null = null;
  let isPlaying = false;
  let isLooping = false;
  let playbackError = '';
  let playerDurationMs = 0;
  let playerCurrentMs = 0;
  let pendingSeekMs: number | null = null;
  let autoplayedMediaPath: string | null = null;
  let timelineViewportStartMs = 0;
  let timelineViewportEndMs = 0;
  let hasManualTimelineZoom = false;
  let timelineDragTarget: TimelineMarker | null = null;
  let timelineElement: HTMLDivElement | null = null;
  let playbackAnimationFrame: number | null = null;
  let pollTimer: ReturnType<typeof setTimeout> | null = null;
  let syncingAssetKey: string | null = null;
  let syncedMediaPath: string | null = null;
  let selectedAsset: UploadAssetView | null = null;
  let selectedAssetKey: string | null = null;
  let mediaListElement: HTMLDivElement | null = null;
  let mediaGroups: Array<{ month: string; assets: UploadAssetView[] }> = [];

  const moveNameById = new Map(data.moves.map((move) => [move.id, move.name ?? move.id]));

  $: selectedAsset = assets.find((asset) => asset.id === selectedAssetId) ?? null;

  $: selectedAssetKey = selectedAsset ? assetSyncKey(selectedAsset) : null;

  $: activeDraftMoveRow = draftMoveRows.find((row) => row.id === activeDraftMoveRowId) ?? null;
  $: activeSavedClip = clipRows.find((clip) => clip.id === activeClipId) ?? null;
  $: activeCountMarkers = activeSavedClip?.countMarkers ?? [];
  $: currentCountMarker = activeSavedClip ? activeCountMarkers[countModeIndex] ?? null : null;
  $: activePreviewCountMarker = activeVisibleCountMarker(activeCountMarkers, playerCurrentMs);

  $: selectedDraftMoveIds = draftMoveRows
    .flatMap((row) => row.moveIds)
    .filter(Boolean);

  $: currentDraftSnapshot = JSON.stringify([
    draftMoveRows.map((row) => [row.id, row.moveIds, row.query, row.startMs, row.endMs]),
    draftStartMs,
    draftEndMs
  ]);

  $: hasDraftChanges = isDraftingMove && currentDraftSnapshot !== draftInitialSnapshot;

  $: hasSaveableDraftChanges =
    hasDraftChanges && draftMoveRows.some((row) => row.moveIds.some((moveId) => moveNameById.has(moveId)) && row.endMs > row.startMs);

  $: if (!isDraftingMove && hasManualTimelineZoom) {
    resetTimelineZoom();
  }

  $: if (!isDraftingMove && isLooping) {
    isLooping = false;
  }

  $: if (selectedAsset && selectedAssetKey && syncingAssetKey !== selectedAssetKey) {
    const mediaPathChanged = syncedMediaPath !== selectedAsset.filePath;
    syncingAssetKey = selectedAssetKey;
    syncedMediaPath = selectedAsset.filePath;
    editDisplayName = selectedAsset.displayName;
    editDancers = selectedAsset.dancers.join(', ');
    editTiming = selectedAsset.timing;
    editContentType = selectedAsset.contentType;
    editEnvironment = selectedAsset.environment;
    editOriginType = selectedAsset.originType;
    editSourceUrl = selectedAsset.sourceUrl ?? '';
    editCreatedAt = dateInputValue(selectedAsset.createdAt);
    editRecordDate = selectedAsset.recordDate ?? '';
    editClassWorkshop = selectedAsset.classWorkshop ?? '';
    editTags = [...selectedAsset.tags];
    editTagDraft = '';
    editNotes = selectedAsset.notes ?? '';
    detectStatus = '';
    publishStatus = '';
    isEditingMetadata = false;
    clipRows = selectedAsset.clips.map((clip) => ({ ...clip, selected: false }));
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
    draftEndMs = playerDurationMs ? Math.min(playerDurationMs, DEFAULT_CLIP_PADDING_MS) : DEFAULT_CLIP_PADDING_MS;
    draftActionStartMs = 0;
    draftActionEndMs = playerDurationMs ? Math.min(playerDurationMs, DEFAULT_CLIP_PADDING_MS) : DEFAULT_CLIP_PADDING_MS;
    autoClipStart = true;
    autoClipEnd = true;
    clipStartContextMs = DEFAULT_CLIP_PADDING_MS;
    clipEndContextMs = DEFAULT_CLIP_PADDING_MS;
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
      activeClipId = data.selectedClipId;
      if (clip) {
        seekPreview(clip.actionStartMs ?? clip.startMs);
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
          clip.label ?? '',
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
    draftMoveRows = draftMoveRows.map((row) => (row.id === rowId ? { ...row, ...patch } : row));
  }

  function createDraftMoveRow(startMs: number, endMs: number): DraftMoveRow {
    return {
      id: createDraftClipId(),
      moveIds: [],
      query: '',
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
      moveIds: [...row.moveIds, normalized],
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
    return clip.label?.trim() || moveNameById.get(clip.moveId) || clip.moveId;
  }

  function readyPublishableClips() {
    return clipRows.filter((clip) => clip.status === 'ready' && clip.outputAssetId && clipPublicationStatus(clip) !== 'modern-published');
  }

  function updateClipLabel(clipId: string, value: string) {
    clipRows = clipRows.map((clip) =>
      clip.id === clipId ? { ...clip, label: value, manuallyNamed: true, updatedAt: new Date().toISOString() } : clip
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
    seekPreview(clip.actionStartMs ?? clip.startMs);
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
    formData.set('classWorkshop', '');
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
        dancers: editDancers,
        timing: editTiming,
        contentType: editContentType,
        environment: editEnvironment,
        originType: editOriginType,
        sourceUrl: editSourceUrl,
        createdAt: editCreatedAt,
        recordDate: editRecordDate,
        classWorkshop: editClassWorkshop,
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
      draftEndMs = Math.min(nextDurationMs, Math.max(draftEndMs, DEFAULT_CLIP_PADDING_MS));
      draftActionEndMs = Math.min(nextDurationMs, Math.max(draftActionEndMs, DEFAULT_CLIP_PADDING_MS));
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
    videoElement.muted = true;

    try {
      await videoElement.play();
    } catch {
      // Browser autoplay policy can still reject this; manual play remains available.
    }
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

    const startMs = clampMs(draftStartMs);
    const endMs = clampMs(draftEndMs);
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

  function resetClipPadding() {
    autoClipStart = true;
    autoClipEnd = true;
    clipStartContextMs = DEFAULT_CLIP_PADDING_MS;
    clipEndContextMs = DEFAULT_CLIP_PADDING_MS;
    draftStartMs = clampClipStartMs(Math.max(0, draftActionStartMs - clipStartContextMs));
    draftEndMs = clampClipEndMs(draftActionEndMs + clipEndContextMs);
  }

  function setDraftBoundary(target: TimelineMarker, valueMs: number, seek = true) {
    const nextValue = clampMs(valueMs);
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

  function timelineOverviewLeft() {
    const duration = inferredTimelineDurationMs();
    return duration ? Math.max(0, Math.min(100, (timelineViewportStartMs / duration) * 100)) : 0;
  }

  function timelineOverviewWidth() {
    const duration = inferredTimelineDurationMs();
    return duration ? Math.max(0, Math.min(100, (timelineViewportDurationMs() / duration) * 100)) : 100;
  }

  function markerLeftStyle(milliseconds: number) {
    return `left: ${percentForMs(milliseconds)}%`;
  }

  function draftMoveRangeStyle(row: DraftMoveRow) {
    return `left: ${percentForMs(row.startMs)}%; width: ${clipPercentWidth(row.startMs, row.endMs)}%`;
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
    const nextValue = timelineMsFromPointer(event);
    timelineDragTarget = target ?? 'playhead';
    setDraftBoundary(timelineDragTarget, nextValue);
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
    timelineDragTarget = null;
    if (releasedTarget && releasedTarget !== 'playhead') {
      seekPreview(draftActionStartMs);
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
      playerDurationMs || actionStart + DEFAULT_CLIP_PADDING_MS,
      actionStart + DEFAULT_CLIP_PADDING_MS
    );
    activeClipId = createDraftClipId();
    isDraftingMove = true;
    draftActionStartMs = actionStart;
    draftActionEndMs = actionEnd;
    const initialRow = createDraftMoveRow(actionStart, actionEnd);
    draftMoveRows = [initialRow];
    activeDraftMoveRowId = initialRow.id;
    draftStartMs = Math.max(0, actionStart - DEFAULT_CLIP_PADDING_MS);
    draftEndMs = Math.min(
      playerDurationMs || actionEnd + DEFAULT_CLIP_PADDING_MS,
      actionEnd + DEFAULT_CLIP_PADDING_MS
    );
    draftStartMs = clampClipStartMs(draftStartMs);
    draftEndMs = clampClipEndMs(draftEndMs);
    autoClipStart = true;
    autoClipEnd = true;
    clipStartContextMs = DEFAULT_CLIP_PADDING_MS;
    clipEndContextMs = DEFAULT_CLIP_PADDING_MS;
    resetTimelineZoom();
    draftInitialSnapshot = JSON.stringify([
      draftMoveRows.map((row) => [row.id, row.moveIds, row.query, row.startMs, row.endMs]),
      draftStartMs,
      draftEndMs
    ]);
  }

  function exitDraftMove() {
    activeClipId = null;
    isDraftingMove = false;
    isLooping = false;
    draftMoveRows = [];
    activeDraftMoveRowId = null;
    draftInitialSnapshot = '';
    lastDraftBoundaryTarget = null;
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

    const existingClip = clipRows.find((clip) => clip.id === activeClipId) ?? null;
    const remainingClips = clipRows.filter((clip) => clip.id !== activeClipId);
    let firstGeneratedClip = true;
    const nextClips = moveRows.flatMap((row) => {
      const clipStartMs = Math.max(0, Math.round(row.startMs - Math.max(clipStartContextMs, CLIP_MOVE_BUFFER_MS)));
      const clipEndMs = Math.max(
        row.endMs + CLIP_MOVE_BUFFER_MS,
        Math.round(row.endMs + Math.max(clipEndContextMs, CLIP_MOVE_BUFFER_MS))
      );

      return row.validMoveIds.map((moveId) => {
        const reusableClip = firstGeneratedClip ? existingClip : null;
        firstGeneratedClip = false;

        return {
          id: reusableClip?.id ?? createDraftClipId(),
          sourceAssetId: selectedAsset.id,
          moveId,
          label: reusableClip?.label ?? null,
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

    return [...remainingClips, ...nextClips];
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
          label: clip.label,
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
    renderStatus = 'Rendering...';
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

    renderStatus = 'Saving clip names...';
    const response = await fetch('/api/upload/clips', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        sourceAssetId: selectedAsset.id,
        clips: clipRows.map((clip) => ({
          id: clip.id,
          moveId: clip.moveId,
          label: clip.label,
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
      renderStatus = payload.error ?? 'Could not save clip names.';
      return;
    }

    clipRows = payload.clips.map((clip: DerivedClip) => ({ ...clip, selected: false }));
    renderStatus = 'Clip names saved.';
    await refreshLibrary(selectedAsset.id);
  }

  async function publishReadyClips() {
    const publishable = readyPublishableClips();
    if (!publishable.length || !selectedAsset) {
      publishStatus = 'Render clips before publishing.';
      return;
    }

    publishStatus = 'Publishing to moves...';
    const response = await fetch('/api/upload/publish', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ clipIds: publishable.map((clip) => clip.id) })
    });
    const payload = await response.json();

    if (!response.ok) {
      publishStatus = payload.error ?? 'Could not publish clips.';
      return;
    }

    publishStatus = `Published ${payload.clips?.length ?? publishable.length} clip${publishable.length === 1 ? '' : 's'} to moves.`;
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
      playerDurationMs || actionStart + DEFAULT_CLIP_PADDING_MS,
      actionStart + DEFAULT_CLIP_PADDING_MS
    );
    const nextRow = createDraftMoveRow(actionStart, actionEnd);
    draftMoveRows = [...draftMoveRows, nextRow];
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

    renderStatus = 'Render complete.';
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
                  {#if !isPlaying}
                    <button
                      class="editor-play-overlay"
                      type="button"
                      aria-label="Play source video"
                      on:click={(event) => {
                        event.stopPropagation();
                        void togglePlayback();
                      }}
                    >
                      Play
                    </button>
                  {/if}
                  <div class="editor-video-controls" on:click={(event) => event.stopPropagation()}>
                    <button type="button" on:click={() => void togglePlayback()}>{isPlaying ? 'Pause' : 'Play'}</button>
                    <span>{formatRoundedSeconds(playerCurrentMs)}s / {formatRoundedSeconds(playerDurationMs)}s</span>
                  </div>
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
                {#if isTimelineZoomed()}
                  <div class="timeline-overview zoomed" aria-hidden="true">
                    <span class="timeline-overview-track">
                      <span
                        class="timeline-overview-window"
                        style={`left: ${timelineOverviewLeft()}%; width: ${timelineOverviewWidth()}%`}
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
                  {#if isDraftingMove}
                    <div
                      class="clip-timeline-selection clip-range"
                      style={`left: ${percentForMs(draftStartMs)}%; width: ${clipPercentWidth(draftStartMs, draftEndMs)}%`}
                    ></div>
                    {#each draftMoveRows as row (row.id)}
                      <div
                        class="clip-timeline-selection move-range"
                        class:active={row.id === activeDraftMoveRowId}
                        class:secondary={row.id !== activeDraftMoveRowId}
                        style={draftMoveRangeStyle(row)}
                        on:dblclick={(event) => editDraftMoveRowFromTimeline(event, row.id)}
                      ></div>
                    {/each}
                  {/if}
                  <div class="clip-timeline-playhead" style={markerLeftStyle(playerCurrentMs)}></div>
                  <div
                    role="button"
                    tabindex={playerDurationMs ? 0 : -1}
                    class="clip-timeline-playhead-handle"
                    style={markerLeftStyle(playerCurrentMs)}
                    aria-label="Drag playback position"
                    title="Playback position"
                    on:pointerdown={(event) => (event.stopPropagation(), startPlayheadDrag(event))}
                  ></div>
                  {#if isDraftingMove}
                  <button
                    type="button"
                    class="clip-timeline-marker clip-marker"
                    style={markerLeftStyle(draftStartMs)}
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
                    style={markerLeftStyle(draftEndMs)}
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
                    style={markerLeftStyle(draftActionStartMs)}
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
                    style={markerLeftStyle(draftActionEndMs)}
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
                    {#if hasSaveableDraftChanges}
                      <button class="timeline-move-action primary" type="button" on:click={() => saveMovesAndQueueRender()}>
                        Save
                      </button>
                    {/if}
                  {:else}
                    <button class="timeline-move-action" type="button" on:click={addMoreMoves}>Edit moves</button>
                  {/if}
                </div>
                {#if isDraftingMove}
                  <div class="clip-draft-form editor-draft-form">
                    <div class="draft-move-header" aria-hidden="true">
                      <span>Start</span>
                      <span>Moves</span>
                      <span></span>
                    </div>
                    {#each draftMoveRows as row (row.id)}
                      <div class="draft-move-row" class:active={row.id === activeDraftMoveRowId}>
                        <div class="move-start-display">
                          <strong>{formatTenthSeconds(row.startMs)}s</strong>
                        </div>
                        <div class="move-link-field">
                          <MovePicker
                            moves={data.moves}
                            selectedIds={row.moveIds}
                            excludedIds={selectedDraftMoveIds}
                            query={row.query}
                            limit={MOVE_SUGGESTION_LIMIT}
                            selectedPlacement="inside"
                            on:focus={() => selectDraftMoveRow(row.id)}
                            on:query={(event) => handleDraftMoveQueryInput(row.id, event.detail.query)}
                            on:select={(event) => addDraftMove(event.detail.moveId, row.id)}
                            on:remove={(event) => removeDraftMove(row.id, event.detail.moveId)}
                          />
                        </div>
                        {#if row.id === activeDraftMoveRowId}
                          <button class="draft-edit-button active" type="button" disabled aria-pressed="true">Editing</button>
                        {:else}
                          <button class="draft-edit-button" type="button" on:click={() => selectDraftMoveRow(row.id)}>Edit</button>
                        {/if}
                      </div>
                    {/each}
                    <button class="draft-add-move-button" type="button" on:click={() => void addMoreMoves()}>Add move</button>
                  </div>
                {/if}
                {#if renderStatus}
                  <p class="timeline-render-status muted">{renderStatus}</p>
                {/if}
              </div>
            </div>

            {#if selectedAsset.clips.length || clipRows.length}
              <div class="meta-card upload-card media-clips-card">
                <div class="panel-header">
                  <div class="panel-heading-row">
                    <h3>Move clips</h3>
                    <button
                      class="header-button"
                      type="button"
                      disabled={!readyPublishableClips().length}
                      on:click={() => void publishReadyClips()}
                    >
                      Publish to moves
                    </button>
                  </div>
                  {#if publishStatus}
                    <p class="muted">{publishStatus}</p>
                  {/if}
                </div>
                {#if clipRows.length}
                  <div class="media-clip-list">
                    {#each clipRows as clip}
                      <div class={`media-clip-row ${clipPublicationClass(clip)}`} class:active={activeClipId === clip.id}>
                        <button
                          class="media-clip-time"
                          type="button"
                          on:click={() => selectSavedClip(clip)}
                        >
                          {formatTenthSeconds(clip.actionStartMs ?? clip.startMs)}s
                        </button>
                        <div class="media-clip-main">
                          <input
                            aria-label={`Clip name for ${clip.moveId}`}
                            value={clipDisplayName(clip)}
                            on:input={(event) => updateClipLabel(clip.id, (event.currentTarget as HTMLInputElement).value)}
                          />
                          <span>{clip.moveId} · {moveNameById.get(clip.moveId) ?? clip.moveId}</span>
                        </div>
                        <div class="content-badge-list media-clip-badges">
                          <ContentBadge status={clipPublicationStatus(clip)} />
                          <ContentBadge status={processingStatusFor(clip.status)} />
                          {#if clip.status === 'failed' && clip.error}
                            <ContentBadge label="Failed" tone="danger" title={clip.error} />
                          {/if}
                        </div>
                        <button class="media-clip-delete" type="button" on:click={() => removeSavedClip(clip.id)}>
                          Delete
                        </button>
                      </div>
                    {/each}
                  </div>
                {:else}
                  <p class="muted">No move tracks. Save clip changes to remove all tracks from this source video.</p>
                {/if}
                {#if activeSavedClip}
                  <div class="clip-tools-panel">
                    <div class="clip-tools-row">
                      <strong>{clipDisplayName(activeSavedClip)}</strong>
                      <button type="button" class:active={isCroppingClip} on:click={() => (isCroppingClip = !isCroppingClip)}>
                        Crop
                      </button>
                      <button type="button" on:click={clearActiveCrop} disabled={!activeSavedClip.cropRect}>Clear crop</button>
                    </div>
                    <div class="clip-tools-row">
                      <label>
                        <span>Timing</span>
                        <select
                          value={activeSavedClip.countTimingPreset}
                          on:change={(event) => setActiveCountPreset((event.currentTarget as HTMLSelectElement).value as CountTimingPreset)}
                        >
                          {#each Object.keys(COUNT_PRESET_SEQUENCES) as preset}
                            <option value={preset}>{presetLabel(preset as CountTimingPreset)}</option>
                          {/each}
                        </select>
                      </label>
                      <label>
                        <span>Marker</span>
                        <select
                          value={activeSavedClip.countOverlayPlacement}
                          on:change={(event) => setActiveCountPlacement((event.currentTarget as HTMLSelectElement).value as CountOverlayPlacement)}
                        >
                          {#each ['top-left', 'top-right', 'bottom-left', 'bottom-right'] as placement}
                            <option value={placement}>{countPlacementLabel(placement as CountOverlayPlacement)}</option>
                          {/each}
                        </select>
                      </label>
                      <button type="button" class:active={countMode === 'placing'} on:click={startCountMode}>Place counts</button>
                      <button type="button" on:click={finishCountMode} disabled={countMode !== 'placing'}>Finish</button>
                      <button type="button" on:click={clearCountMarkers} disabled={!activeSavedClip.countMarkers.length}>Clear counts</button>
                    </div>
                    {#if countMode === 'placing' && currentCountMarker}
                      <div class="clip-count-workflow">
                        <strong>{currentCountMarker.count}</strong>
                        <span>{countModeIndex + 1} / {activeSavedClip.countMarkers.length}</span>
                        <button type="button" on:click={placeCurrentCount}>Place</button>
                      </div>
                    {/if}
                    {#if activeSavedClip.countMarkers.length}
                      <div class="clip-count-marker-list">
                        {#each activeSavedClip.countMarkers as marker, index}
                          <button
                            type="button"
                            class:active={index === countModeIndex && countMode === 'placing'}
                            on:click={() => {
                              countModeIndex = index;
                              seekPreview(marker.ms);
                            }}
                          >
                            {marker.count} · {formatTenthSeconds(marker.ms)}s
                          </button>
                          <label class="clear-count-toggle">
                            <input type="checkbox" checked={marker.clear} on:change={() => toggleCountClear(marker.id)} />
                            <span>Clear</span>
                          </label>
                        {/each}
                      </div>
                    {/if}
                  </div>
                {/if}
                <div class="upload-actions">
                  <button type="button" on:click={() => void saveClipLabelChanges()}>Save clip changes</button>
                </div>
              </div>
            {/if}

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
                    <input bind:value={editDancers} placeholder="Comma separated" />
                  </label>
                  <div class="segmented-field">
                    <span class="segmented-label">Source</span>
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
                    <span>Class/workshop</span>
                    <input list="class-workshop-options" bind:value={editClassWorkshop} />
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
                  <dd>{selectedAsset.dancers.length ? selectedAsset.dancers.join(', ') : '—'}</dd>
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
                  {#if selectedAsset.classWorkshop}
                    <dt>Class/workshop</dt>
                    <dd>{selectedAsset.classWorkshop}</dd>
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

  <datalist id="class-workshop-options">
    {#each suggestions.classWorkshops as classWorkshop}
      <option value={classWorkshop}></option>
    {/each}
  </datalist>
  <datalist id="media-tag-options">
    {#each suggestions.tags as tag}
      <option value={tag}></option>
    {/each}
  </datalist>
</div>
