<script lang="ts">
  import { browser } from '$app/environment';
  import { beforeNavigate } from '$app/navigation';
  import { flip } from 'svelte/animate';
  import { cubicInOut, cubicOut } from 'svelte/easing';
  import { onDestroy, onMount, tick } from 'svelte';
  import { fade, fly } from 'svelte/transition';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import EntityPicker from '$lib/components/EntityPicker.svelte';
  import { publicationStatusFor, processingStatusFor } from '$lib/content-status';
  import type { EntityPickerTemplate } from '$lib/components/entity-picker';
  import {
    applyVideoAudioPreference,
    hasActiveMutedVideoPreference,
    saveVideoAudioPreferenceFromElement
  } from '$lib/video-audio-preference';
  import { snapMoveBoundaryForDrag } from '$lib/timeline-snapping.js';
  import {
    clampTimelineViewport,
    msFromTimelineRatio,
    timelineZoomed,
    zoomTimelineViewport
  } from '$lib/timeline-zoom.js';
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
  type EditorMoveContext = {
    previous: EditorMoveRow | null;
    current: EditorMoveRow | null;
    next: EditorMoveRow | null;
  };
  type TimelineMoveContextEntry = {
    key: string;
    label: string;
    row?: EditorMoveRow;
    empty?: boolean;
  };
  type TimelineMoveContext = {
    previous: TimelineMoveContextEntry | null;
    current: TimelineMoveContextEntry | null;
    next: TimelineMoveContextEntry | null;
  };
  type TimelineMarker = 'clipStart' | 'clipEnd' | 'moveStart' | 'moveEnd' | 'playhead';
  type CountModeStep = 'idle' | 'placing';
  type ClipChangeState = 'new' | 'edited';
  type TimelinePointer = { clientX: number; clientY: number };
  type TimelineViewport = { startMs: number; endMs: number };
  type TimelineActionButton = {
    key: string;
    label: string;
    className?: string;
    active?: boolean;
    ariaLabel?: string;
    ariaPressed?: boolean;
    title?: string;
    onClick: () => void | Promise<void>;
  };
  type FullscreenDocument = Document & {
    webkitFullscreenElement?: Element | null;
    webkitExitFullscreen?: () => Promise<void> | void;
  };
  type FullscreenElement = HTMLElement & {
    webkitRequestFullscreen?: () => Promise<void> | void;
  };
  type FullscreenVideoElement = HTMLVideoElement & {
    webkitEnterFullscreen?: () => void;
  };
  const MOVE_SUGGESTION_LIMIT = 8;
  const CLIP_MOVE_BUFFER_MS = 500;
  const DEFAULT_MOVE_DURATION_MS = 2500;
  const DEFAULT_CLIP_HEAD_PADDING_MS = DEFAULT_MOVE_DURATION_MS;
  const DEFAULT_CLIP_TAIL_PADDING_MS = Math.round(DEFAULT_MOVE_DURATION_MS / 2);
  const MOVE_BOUNDARY_SNAP_TOLERANCE_PX = 8;
  const PLAYBACK_CONTEXT_WINDOW_MS = 2500;
  const MEDIA_MOTION_SHORT_MS = 320;
  const MEDIA_MOTION_MEDIUM_MS = 700;
  const MEDIA_MOTION_REVEAL_DELAY_MS = 120;
  const TIMELINE_WHEEL_ZOOM_DELTA_SCALE = 0.001375;
  const TIMELINE_SMOOTH_ZOOM_EASE = 0.28;
  const TIMELINE_SMOOTH_ZOOM_SETTLE_MS = 2;
  const TIMELINE_PINCH_ZOOM_DAMPING = 0.22;
  const TIMELINE_TOUCH_PINCH_ZOOM_DAMPING = TIMELINE_PINCH_ZOOM_DAMPING * 10;
  const TIMELINE_PINCH_SCALE_MIN = 0.05;
  const TIMELINE_PINCH_SCALE_MAX = 20;
  const TIMELINE_ZOOMING_MS = 180;
  const draftMovePickerTemplate: EntityPickerTemplate = {
    key: 'media-edit-draft-move',
    kind: 'move',
    showHeader: false,
    placeholder: 'Search moves by id or name',
    addPlaceholder: 'Add another move',
    ariaLabel: 'Search moves by id or name',
    mode: 'singleEdit',
    createPolicy: 'draftMove',
    valueSource: 'id',
    density: 'compact',
    maxSelected: 1,
    showId: false
  };
  const startPositionPickerTemplate: EntityPickerTemplate = {
    key: 'media-edit-start-position',
    kind: 'searchable',
    showHeader: false,
    placeholder: 'Start',
    addPlaceholder: 'Start',
    ariaLabel: 'Start position',
    mode: 'singleEdit',
    createPolicy: 'local',
    valueSource: 'id',
    density: 'compact',
    createLabel: 'Create position'
  };
  const endPositionPickerTemplate: EntityPickerTemplate = {
    key: 'media-edit-end-position',
    kind: 'searchable',
    showHeader: false,
    placeholder: 'End',
    addPlaceholder: 'End',
    ariaLabel: 'End position',
    mode: 'singleEdit',
    createPolicy: 'local',
    valueSource: 'id',
    density: 'compact',
    createLabel: 'Create position'
  };
  const mediaDancerPickerTemplate: EntityPickerTemplate = {
    key: 'media-edit-dancers',
    kind: 'searchable',
    showHeader: false,
    placeholder: 'Search dancers',
    addPlaceholder: 'Add dancer',
    ariaLabel: 'Dancers',
    mode: 'multiEdit',
    createPolicy: 'none',
    valueSource: 'label',
    density: 'compact'
  };
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
  let previousAudibleVideoVolume = 1;
  let isVolumeOpen = false;
  let isVideoFullscreen = false;
  let isLooping = false;
  let isLoopingWithPadding = true;
  let isZoomLooping = false;
  let isLoopSuppressedByTimelineSeek = false;
  let isTimelineZooming = false;
  let playbackError = '';
  let playerDurationMs = 0;
  let playerCurrentMs = 0;
  let timelineInferredDurationMs = 0;
  let timelineIsZoomed = false;
  let timelineZoomControlsVisible = false;
  let pendingSeekMs: number | null = null;
  let autoplayedMediaPath: string | null = null;
  let timelineViewportStartMs = 0;
  let timelineViewportEndMs = 0;
  let timelineZoomTargetViewport: TimelineViewport | null = null;
  let hasManualTimelineZoom = false;
  let timelineDragTarget: TimelineMarker | null = null;
  $: isDraggingTimelineMarker = Boolean(timelineDragTarget && timelineDragTarget !== 'playhead');
  let timelineDragPreviousMs: number | null = null;
  let timelineDragSnapConsumed = false;
  let timelineDragCaptureElement: HTMLElement | null = null;
  let timelineDragPointerId: number | null = null;
  let timelineTouchPointers = new Map<number, TimelinePointer>();
  let timelinePinchActive = false;
  let timelinePinchStartDistancePx = 0;
  let timelinePinchStartViewport: TimelineViewport | null = null;
  let timelinePinchAnchorMs = 0;
  let timelinePinchRestorePlayheadMs: number | null = null;
  let timelinePinchRestoreLoopSuppression: boolean | null = null;
  let resumePlaybackAfterTimelineDrag = false;
  let timelineElement: HTMLDivElement | null = null;
  let draftMoveRowWindowElement: HTMLDivElement | null = null;
  let timelineWidthPx = 0;
  let timelineResizeObserver: ResizeObserver | null = null;
  let playbackAnimationFrame: number | null = null;
  let timelinePromotionFrame: number | null = null;
  let timelinePromotionTimer: ReturnType<typeof setTimeout> | null = null;
  let timelineZoomAnimationFrame: number | null = null;
  let timelineZoomingTimer: ReturnType<typeof setTimeout> | null = null;
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
  let targetEditorMoveRows: EditorMoveRow[] = [];
  let renderedEditorMoveRows: EditorMoveRow[] = [];
  let visibleEditorMoveRows: EditorMoveRow[] = [];
  let visibleEditorDraftMoveRows: DraftMoveRow[] = [];
  let editMoveContext: EditorMoveContext = { previous: null, current: null, next: null };
  let timelineMoveContext: TimelineMoveContext = { previous: null, current: null, next: null };
  let currentPlaybackMove: ClipWithUi | null = null;
  let previousPlaybackMove: ClipWithUi | null = null;
  let nextPlaybackMove: ClipWithUi | null = null;
  let visiblePreviousPlaybackMove: ClipWithUi | null = null;
  let visibleNextPlaybackMove: ClipWithUi | null = null;
  let timelineActionButtons: TimelineActionButton[] = [];
  let timelineZoomButtons: TimelineActionButton[] = [];
  let showPlaybackMoveContext = false;
  let prefersReducedMotion = false;
  let timelinePromotingDraftRowId: string | null = null;
  let timelinePromotionAtSavedLane = false;
  let timelineEditorChromeVisible = true;
  let dancerOptions = data.dancerOptions.map((dancer) => ({ id: dancer, label: dancer }));
  let positionPickerOptions = data.positionOptions.map((position) => ({ id: position.id, label: position.label }));

  let moveNameById = new Map<string, string>();
  let draftMoveIds = new Set<string>();
  $: moveNameById = new Map(availableMoves.map((move) => [move.id, move.name ?? move.id]));
  $: draftMoveIds = new Set(availableMoves.filter((move) => move.isDraft).map((move) => normalizeMoveId(move.id)));

  $: selectedAsset = assets.find((asset) => asset.id === selectedAssetId) ?? null;

  $: selectedAssetKey = selectedAsset ? assetSyncKey(selectedAsset) : null;

  $: activeDraftMoveRow = draftMoveRows.find((row) => row.id === activeDraftMoveRowId) ?? null;
  $: activeSavedClip = clipRows.find((clip) => clip.id === activeClipId) ?? null;
  $: activeDraftOriginalClipIds = new Set(draftMoveRows.map((row) => row.originalClipId).filter((id): id is string => Boolean(id)));
  $: activeCountMarkers = activeSavedClip?.countMarkers ?? [];
  $: currentCountMarker = activeSavedClip ? activeCountMarkers[countModeIndex] ?? null : null;
  $: activePreviewCountMarker = activeVisibleCountMarker(activeCountMarkers, playerCurrentMs);
  $: visibleSavedTimelineClips = isDraftingMove ? [] : clipRows;
  $: savedSnapBoundaryClips =
    isDraftingMove && activeDraftOriginalClipIds.size
      ? clipRows.filter((clip) => !activeDraftOriginalClipIds.has(clip.id))
      : clipRows;
  $: editorMoveRows = editorRowsForDisplay(clipRows, draftMoveRows, isDraftingMove);
  $: targetEditorMoveRows = visibleEditorRowsForDisplay(
    editorMoveRows,
    isDraftingMove,
    activeDraftMoveRowId,
    activeClipId
  );
  $: editMoveContext = editMoveContextForDisplay(editorMoveRows, isDraftingMove, activeDraftMoveRowId, activeClipId);
  $: syncRenderedEditorMoveRows(targetEditorMoveRows);
  $: visibleEditorMoveRows = renderedEditorMoveRows;
  $: visibleEditorDraftMoveRows = visibleEditorMoveRows
    .filter((item): item is { kind: 'draft'; key: string; row: DraftMoveRow } => item.kind === 'draft')
    .map((item) => item.row);
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
  $: timelineMoveContext = isDraftingMove
    ? editTimelineMoveContext(editMoveContext)
    : playbackTimelineMoveContext(currentPlaybackMove, visiblePreviousPlaybackMove, visibleNextPlaybackMove);
  $: showPlaybackMoveContext = isDraftingMove ? Boolean(timelineMoveContext.current) : playbackMoveClips.length > 0;
  $: timelineScaleKey = `${isDraftingMove ? 'editing' : 'full'}:${playerDurationMs}:${timelineViewportStartMs}:${timelineViewportEndMs}`;
  $: timelineInferredDurationMs = Math.max(
    playerDurationMs,
    playerCurrentMs,
    draftEndMs,
    draftActionEndMs,
    draftMoveRows.reduce((max, row) => Math.max(max, row.startMs, row.endMs), 0),
    clipRows.reduce((max, clip) => Math.max(max, clip.endMs, clip.actionEndMs ?? 0, clip.startMs, clip.actionStartMs ?? 0), 0)
  );
  $: timelineIsZoomed = timelineZoomed({
    startMs: timelineViewportStartMs,
    endMs: timelineViewportEndMs,
    durationMs: timelineInferredDurationMs
  });
  $: timelineZoomControlsVisible = timelineIsZoomed || hasManualTimelineZoom || isZoomLooping;

  function motionDuration(milliseconds: number) {
    return prefersReducedMotion ? 0 : milliseconds;
  }

  function buildTimelineActionButtons(state: {
    isDraftingMove: boolean;
    hasActiveDraftMoveRow: boolean;
    hasSaveableDraftChanges: boolean;
    hasUnsavedClipRowChanges: boolean;
  }): TimelineActionButton[] {
    const actions: TimelineActionButton[] = [];

    if (state.isDraftingMove) {
      actions.push(
        { key: 'exit-moves', label: 'Exit', onClick: exitDraftMove },
        { key: 'add-move', label: 'Add move', onClick: () => void addMoreMoves() }
      );

      if (state.hasActiveDraftMoveRow) {
        actions.push({ key: 'add-bound-move', label: 'Add bound move', onClick: () => addBoundMove() });
      }

      if (state.hasSaveableDraftChanges) {
        actions.push({ key: 'save-moves', label: 'Save', className: 'primary', onClick: () => saveMovesAndQueueRender() });
      }
    } else {
      actions.push({ key: 'edit-moves', label: 'Edit moves', onClick: () => void addMoreMoves() });
    }

    if (state.hasUnsavedClipRowChanges) {
      actions.push({
        key: 'save-clip-changes',
        label: 'Save clip changes',
        className: 'primary',
        onClick: () => void saveClipLabelChanges()
      });
    }

    return actions;
  }

  function buildTimelineZoomButtons(zoomLooping: boolean): TimelineActionButton[] {
    return [
      {
        key: 'reset-zoom',
        label: 'Reset zoom',
        className: 'zoom-control',
        ariaLabel: 'Reset timeline zoom',
        title: 'Reset timeline zoom',
        onClick: resetTimelineZoom
      },
      {
        key: 'loop-zoom',
        label: 'Loop zoom',
        className: 'zoom-control',
        active: zoomLooping,
        ariaLabel: 'Loop zoom window',
        ariaPressed: zoomLooping,
        title: 'Loop zoom window',
        onClick: toggleZoomLoop
      }
    ];
  }

  function clearTimelinePromotionTimers() {
    if (timelinePromotionFrame !== null && browser) {
      cancelAnimationFrame(timelinePromotionFrame);
    }
    timelinePromotionFrame = null;

    if (timelinePromotionTimer !== null) {
      clearTimeout(timelinePromotionTimer);
    }
    timelinePromotionTimer = null;
  }

  function resetTimelinePromotion() {
    clearTimelinePromotionTimers();
    timelinePromotingDraftRowId = null;
    timelinePromotionAtSavedLane = false;
    timelineEditorChromeVisible = true;
  }

  async function beginSavedClipTimelinePromotion(rowId: string) {
    clearTimelinePromotionTimers();
    timelinePromotingDraftRowId = rowId;
    timelinePromotionAtSavedLane = true;
    timelineEditorChromeVisible = false;

    if (!browser || prefersReducedMotion) {
      timelinePromotionAtSavedLane = false;
      timelineEditorChromeVisible = true;
      timelinePromotingDraftRowId = null;
      return;
    }

    await tick();
    if (timelinePromotingDraftRowId !== rowId) {
      return;
    }

    timelinePromotionFrame = requestAnimationFrame(() => {
      if (timelinePromotingDraftRowId !== rowId) {
        timelinePromotionFrame = null;
        return;
      }

      timelinePromotionFrame = null;
      timelinePromotionAtSavedLane = false;
      timelinePromotionTimer = setTimeout(() => {
        if (timelinePromotingDraftRowId !== rowId) {
          timelinePromotionTimer = null;
          return;
        }

        timelinePromotionTimer = null;
        timelineEditorChromeVisible = true;
        timelinePromotingDraftRowId = null;
      }, motionDuration(MEDIA_MOTION_MEDIUM_MS + MEDIA_MOTION_REVEAL_DELAY_MS));
    });
  }

  function commitRenderedEditorMoveRows(rows: EditorMoveRow[]) {
    renderedEditorMoveRows = rows;
  }

  function syncRenderedEditorMoveRows(rows: EditorMoveRow[]) {
    commitRenderedEditorMoveRows(rows);
  }
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
  $: timelineActionButtons = buildTimelineActionButtons({
    isDraftingMove,
    hasActiveDraftMoveRow: Boolean(activeDraftMoveRow),
    hasSaveableDraftChanges,
    hasUnsavedClipRowChanges
  });
  $: timelineZoomButtons = buildTimelineZoomButtons(isZoomLooping);

  beforeNavigate((navigation) => {
    if (navigation.willUnload || !hasDraftChanges) {
      return;
    }

    if (!confirmDiscardDraftMoveChanges()) {
      navigation.cancel();
    }
  });

  $: if (!isDraftingMove && isLooping) {
    isLooping = false;
    isLoopSuppressedByTimelineSeek = false;
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
      isZoomLooping = false;
      isLoopSuppressedByTimelineSeek = false;
      resetTimelinePinch();
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
          clip.actionOutputFilePath ?? '',
          clip.lowResOutputFilePath ?? '',
          clip.lowResPaddedOutputFilePath ?? '',
          clip.publishedAssetId ?? '',
          clip.publishedActionOutputFilePath ?? '',
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
    void scrollMoveEditorRowIntoView({ draftRowId: row.id });
  }

  async function scrollMoveEditorRowIntoView(target: { draftRowId?: string; clipId?: string }) {
    await tick();
    if (!draftMoveRowWindowElement) {
      return;
    }

    const selector = target.draftRowId
      ? `[data-draft-row-id="${CSS.escape(target.draftRowId)}"]`
      : target.clipId
        ? `[data-clip-row-id="${CSS.escape(target.clipId)}"]`
        : '';
    if (!selector) {
      return;
    }

    const rowElement = draftMoveRowWindowElement.querySelector<HTMLElement>(selector);
    if (!rowElement) {
      return;
    }

    const viewportRect = draftMoveRowWindowElement.getBoundingClientRect();
    const rowRect = rowElement.getBoundingClientRect();
    const viewportPadding = 6;

    if (rowRect.top < viewportRect.top + viewportPadding) {
      draftMoveRowWindowElement.scrollTop -= viewportRect.top + viewportPadding - rowRect.top;
    } else if (rowRect.bottom > viewportRect.bottom - viewportPadding) {
      draftMoveRowWindowElement.scrollTop += rowRect.bottom - (viewportRect.bottom - viewportPadding);
    }
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

  function upsertPositionPickerOption(position: { id: string; label: string }) {
    positionPickerOptions = [
      ...positionPickerOptions.filter((option) => option.id !== position.id),
      { id: position.id, label: position.label }
    ].sort((left, right) => left.label.localeCompare(right.label, undefined, { sensitivity: 'base' }));
  }

  async function createDraftPositionFromQuery(rowId: string, field: 'start' | 'end', value: string) {
    const label = value.trim();
    if (!label) {
      return;
    }

    activeDraftMoveRowId = rowId;
    saveStatus = `Creating position "${label}"...`;

    const response = await fetch('/api/positions', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ label })
    });
    const payload = await response.json();

    if (!response.ok) {
      saveStatus = payload.error ?? 'Could not create position.';
      return;
    }

    upsertPositionPickerOption(payload.position);
    selectDraftPosition(rowId, field, payload.position.id);
    saveStatus = `Created position "${payload.position.label}".`;
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
      return clips.map((clip) => ({ kind: 'saved', key: savedEditorRowKey(clip.id), clip }));
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
        ? { kind: 'draft', key: savedEditorRowKey(clip.id), row: draftRow }
        : { kind: 'saved', key: savedEditorRowKey(clip.id), clip };
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

  function savedEditorRowKey(clipId: string) {
    return `clip-${clipId}`;
  }

  function visibleEditorRowsForDisplay(
    rows: EditorMoveRow[],
    editing: boolean,
    activeDraftRowId: string | null,
    activeSavedClipId: string | null
  ) {
    if (!editing) {
      return rows;
    }

    const activeRow = rows.find((row) => editorRowIsActive(row, activeDraftRowId, activeSavedClipId)) ?? null;

    return activeRow ? [activeRow] : rows.slice(0, 1);
  }

  function editMoveContextForDisplay(
    rows: EditorMoveRow[],
    editing: boolean,
    activeDraftRowId: string | null,
    activeSavedClipId: string | null
  ): EditorMoveContext {
    if (!editing || !rows.length) {
      return { previous: null, current: null, next: null };
    }

    const activeIndex = rows.findIndex((row) => editorRowIsActive(row, activeDraftRowId, activeSavedClipId));
    const currentIndex = activeIndex >= 0 ? activeIndex : 0;

    return {
      previous: rows[currentIndex - 1] ?? null,
      current: rows[currentIndex] ?? null,
      next: rows[currentIndex + 1] ?? null
    };
  }

  function editorRowIsActive(row: EditorMoveRow, activeDraftRowId: string | null, activeSavedClipId: string | null) {
    if (row.kind === 'draft') {
      return row.row.id === activeDraftRowId || Boolean(activeSavedClipId && row.row.originalClipId === activeSavedClipId);
    }

    return row.clip.id === activeSavedClipId;
  }

  function editorRowStartMs(row: EditorMoveRow) {
    return row.kind === 'draft' ? row.row.startMs : clipActionStartMs(row.clip);
  }

  function editorRowEndMs(row: EditorMoveRow) {
    return row.kind === 'draft' ? row.row.endMs : clipActionEndMs(row.clip);
  }

  function playbackTimelineContextEntry(clip: ClipWithUi): TimelineMoveContextEntry {
    return {
      key: clip.id,
      label: playbackMoveName(clip)
    };
  }

  function editorTimelineContextEntry(row: EditorMoveRow): TimelineMoveContextEntry {
    if (row.kind === 'saved') {
      return {
        key: row.key,
        label: playbackMoveName(row.clip),
        row
      };
    }

    const moveId = row.row.moveIds[0] ?? '';
    return {
      key: row.key,
      label: moveId ? moveNameById.get(moveId) || moveId : row.row.query.trim() || 'New move',
      row
    };
  }

  function playbackTimelineMoveContext(
    current: ClipWithUi | null,
    previous: ClipWithUi | null,
    next: ClipWithUi | null
  ): TimelineMoveContext {
    return {
      previous: previous ? playbackTimelineContextEntry(previous) : null,
      current: current ? playbackTimelineContextEntry(current) : { key: 'empty', label: '—', empty: true },
      next: next ? playbackTimelineContextEntry(next) : null
    };
  }

  function editTimelineMoveContext(context: EditorMoveContext): TimelineMoveContext {
    return {
      previous: context.previous ? editorTimelineContextEntry(context.previous) : null,
      current: context.current ? editorTimelineContextEntry(context.current) : null,
      next: context.next ? editorTimelineContextEntry(context.next) : null
    };
  }

  function timelineContextActionLabel(position: 'previous' | 'next', entry: TimelineMoveContextEntry) {
    return `Edit ${position} move ${entry.label}`;
  }

  function openEditorContextRow(row: EditorMoveRow | null) {
    if (!row) {
      return;
    }

    if (row.kind === 'draft') {
      selectDraftMoveRow(row.row.id);
      return;
    }

    openSavedClipEditor(row.clip);
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
    if (!confirmDiscardDraftMoveChanges()) {
      return;
    }

    const actionStart = clipActionStartMs(clip);
    const actionEnd = clipActionEndMs(clip);
    const rows = [clip].map((entry) => ({
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
    autoClipStart = true;
    autoClipEnd = true;
    isLooping = true;
    isLoopingWithPadding = true;
    isZoomLooping = false;
    isLoopSuppressedByTimelineSeek = false;
    countMode = 'idle';
    countModeIndex = 0;
    isCroppingClip = false;
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
    void beginSavedClipTimelinePromotion(row.id);
    void scrollMoveEditorRowIntoView({ draftRowId: row.id });
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

  function confirmDiscardDraftMoveChanges() {
    if (!hasDraftChanges || !browser) {
      return true;
    }

    return window.confirm('Discard unsaved draft move edits?');
  }

  function selectAsset(assetId: string) {
    if (assetId !== selectedAssetId && !confirmDiscardDraftMoveChanges()) {
      return;
    }

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

    const nextSelectedAsset =
      nextSelectedAssetId && assets.some((asset: UploadAssetView) => asset.id === nextSelectedAssetId)
        ? assets.find((asset: UploadAssetView) => asset.id === nextSelectedAssetId) ?? null
        : assets[0] ?? null;

    selectedAssetId = nextSelectedAsset?.id ?? null;
    syncingAssetKey = null;
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
    return timelineInferredDurationMs;
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
      hasManualTimelineZoom = false;
      return;
    }

    if (timelineViewportEndMs > timelineDurationMs) {
      timelineViewportEndMs = timelineDurationMs;
    }

    if (!hasManualTimelineZoom) {
      timelineViewportStartMs = 0;
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
    if (videoElement.volume > 0) {
      previousAudibleVideoVolume = videoElement.volume;
    }
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

  function toggleMuted() {
    if (!videoElement) {
      return;
    }

    if (videoElement.muted || videoElement.volume === 0) {
      if (videoElement.volume === 0) {
        videoElement.volume = previousAudibleVideoVolume || 1;
      }
      videoElement.muted = false;
    } else {
      videoElement.muted = true;
    }

    handleAudioPreferenceChange();
  }

  function toggleVolumeOpen() {
    isVolumeOpen = !isVolumeOpen;
  }

  function syncVideoFullscreenState() {
    if (!browser || !videoElement) {
      isVideoFullscreen = false;
      return;
    }

    const fullscreenDocument = document as FullscreenDocument;
    const fullscreenElement = document.fullscreenElement ?? fullscreenDocument.webkitFullscreenElement ?? null;
    isVideoFullscreen = fullscreenElement === videoElement;
  }

  async function toggleVideoFullscreen() {
    if (!videoElement) {
      return;
    }

    const fullscreenDocument = document as FullscreenDocument;
    const fullscreenElement = document.fullscreenElement ?? fullscreenDocument.webkitFullscreenElement ?? null;
    const fullscreenVideo = videoElement as FullscreenVideoElement;

    playbackError = '';

    try {
      if (fullscreenElement === videoElement) {
        if (document.exitFullscreen) {
          await document.exitFullscreen();
        } else {
          await fullscreenDocument.webkitExitFullscreen?.();
        }
        syncVideoFullscreenState();
        return;
      }

      if (videoElement.requestFullscreen) {
        await videoElement.requestFullscreen();
      } else if ((videoElement as FullscreenElement).webkitRequestFullscreen) {
        await (videoElement as FullscreenElement).webkitRequestFullscreen?.();
      } else if (fullscreenVideo.webkitEnterFullscreen) {
        fullscreenVideo.webkitEnterFullscreen();
      } else {
        playbackError = 'Fullscreen is not available for this video.';
        return;
      }

      syncVideoFullscreenState();
    } catch (error) {
      playbackError = error instanceof Error ? error.message : 'Fullscreen could not start.';
    }
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
    if (isZoomLooping) {
      ensureTimelineViewport();
      if (!isTimelineZoomed()) {
        return null;
      }

      const startMs = clampMs(timelineViewportStartMs);
      const endMs = clampMs(timelineViewportEndMs);
      if (endMs <= startMs + 50) {
        return null;
      }

      return { startMs, endMs };
    }

    if (!isDraftingMove || !isLooping) {
      return null;
    }

    const startMs = clampMs(isLoopingWithPadding ? draftStartMs : draftActionStartMs);
    const endMs = clampMs(isLoopingWithPadding ? draftEndMs : draftActionEndMs);
    if (endMs <= startMs + 50) {
      return null;
    }

    return { startMs, endMs };
  }

  function isInsideLoopRange(milliseconds: number, range: { startMs: number; endMs: number }) {
    return milliseconds >= range.startMs && milliseconds < range.endMs;
  }

  function markLoopSuppressionForTimelineSeek(milliseconds: number) {
    const range = activeLoopRange();
    isLoopSuppressedByTimelineSeek = Boolean(range && !isInsideLoopRange(milliseconds, range));
  }

  function enforceLoopAt(milliseconds: number) {
    if ((!isLooping && !isZoomLooping) || !videoElement) {
      isLoopSuppressedByTimelineSeek = false;
      return false;
    }

    const range = activeLoopRange();
    if (!range) {
      isLooping = false;
      isZoomLooping = false;
      isLoopSuppressedByTimelineSeek = false;
      return false;
    }

    if (isLoopSuppressedByTimelineSeek) {
      if (isInsideLoopRange(milliseconds, range)) {
        isLoopSuppressedByTimelineSeek = false;
      } else {
        return false;
      }
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
      isLoopSuppressedByTimelineSeek = false;
      return;
    }

    isLooping = !isLooping;
    isLoopSuppressedByTimelineSeek = false;
    if (!isLooping) {
      return;
    }

    isZoomLooping = false;
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

  function toggleZoomLoop() {
    if (!isTimelineZoomed()) {
      isZoomLooping = false;
      isLoopSuppressedByTimelineSeek = false;
      return;
    }

    isZoomLooping = !isZoomLooping;
    isLoopSuppressedByTimelineSeek = false;
    if (!isZoomLooping) {
      return;
    }

    isLooping = false;
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
      markLoopSuppressionForTimelineSeek(nextValue);
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
    return timelineIsZoomed;
  }

  function shouldShowTimelineZoomControls() {
    return timelineZoomControlsVisible;
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

  function isDraftMoveId(moveId: string | null | undefined) {
    return Boolean(moveId && draftMoveIds.has(normalizeMoveId(moveId)));
  }

  function savedClipUsesDraftMove(clip: DerivedClip) {
    return isDraftMoveId(clip.moveId);
  }

  function draftRowUsesDraftMove(row: DraftMoveRow) {
    return row.moveIds.some((moveId) => isDraftMoveId(moveId));
  }

  function savedClipRangeStyle(clip: DerivedClip, scaleKey = '') {
    const startMs = clip.actionStartMs ?? clip.startMs;
    const endMs = clip.actionEndMs ?? clip.endMs;
    return timelineRangeStyle(startMs, endMs, scaleKey);
  }

  function timelineMsFromClientX(clientX: number) {
    if (!timelineElement || !inferredTimelineDurationMs()) {
      return 0;
    }

    ensureTimelineViewport();
    const bounds = timelineElement.getBoundingClientRect();
    const ratio = Math.max(0, Math.min(1, (clientX - bounds.left) / bounds.width));
    return msFromTimelineRatio({
      ratio,
      viewportStartMs: timelineViewportStartMs,
      viewportEndMs: timelineViewportEndMs
    });
  }

  function timelineMsFromPointer(event: PointerEvent) {
    return timelineMsFromClientX(event.clientX);
  }

  function editDraftMoveRowFromTimeline(event: MouseEvent, rowId: string) {
    event.stopPropagation();
    selectDraftMoveRow(rowId);
  }

  function resetTimelinePinch() {
    timelineTouchPointers.clear();
    timelinePinchActive = false;
    timelinePinchStartDistancePx = 0;
    timelinePinchStartViewport = null;
    timelinePinchAnchorMs = 0;
    timelinePinchRestorePlayheadMs = null;
    timelinePinchRestoreLoopSuppression = null;
    timelineZoomTargetViewport = null;
  }

  function updateTimelineTouchPointer(event: PointerEvent) {
    if (event.pointerType !== 'touch') {
      return;
    }

    timelineTouchPointers.set(event.pointerId, {
      clientX: event.clientX,
      clientY: event.clientY
    });
  }

  function timelineTouchPair() {
    const pointers = Array.from(timelineTouchPointers.values());
    if (pointers.length < 2) {
      return null;
    }

    return [pointers[0], pointers[1]] as const;
  }

  function timelinePointerDistance(left: TimelinePointer, right: TimelinePointer) {
    return Math.hypot(left.clientX - right.clientX, left.clientY - right.clientY);
  }

  function beginTimelinePinch() {
    const pair = timelineTouchPair();
    const timelineDurationMs = inferredTimelineDurationMs();
    if (!pair || !timelineElement || !timelineDurationMs) {
      return false;
    }

    ensureTimelineViewport();
    const distance = timelinePointerDistance(pair[0], pair[1]);
    if (distance <= 0) {
      return false;
    }

    restoreTimelinePinchPlayhead();

    timelinePinchActive = true;
    timelineDragTarget = null;
    timelineDragPreviousMs = null;
    timelineDragSnapConsumed = false;
    resumePlaybackAfterTimelineDrag = false;
    timelinePinchStartDistancePx = distance;
    timelinePinchStartViewport = {
      startMs: timelineViewportStartMs,
      endMs: timelineViewportEndMs
    };
    timelinePinchAnchorMs = timelineMsFromClientX((pair[0].clientX + pair[1].clientX) / 2);
    return true;
  }

  function restoreTimelinePinchPlayhead() {
    if (timelinePinchRestorePlayheadMs === null) {
      return;
    }

    seekPreview(timelinePinchRestorePlayheadMs);
    if (timelinePinchRestoreLoopSuppression !== null) {
      isLoopSuppressedByTimelineSeek = timelinePinchRestoreLoopSuppression;
    }
  }

  function updateTimelinePinch() {
    const pair = timelineTouchPair();
    if (!timelinePinchActive || !pair || !timelinePinchStartViewport || !timelinePinchStartDistancePx) {
      return;
    }

    const distance = timelinePointerDistance(pair[0], pair[1]);
    if (distance <= 0) {
      return;
    }

    const rawScale = timelinePinchStartDistancePx / distance;
    const touchScale = 1 + (rawScale - 1) * TIMELINE_TOUCH_PINCH_ZOOM_DAMPING;
    const dampedScale = Math.max(TIMELINE_PINCH_SCALE_MIN, Math.min(TIMELINE_PINCH_SCALE_MAX, touchScale));
    zoomTimelineAround(timelinePinchAnchorMs, dampedScale, timelinePinchStartViewport);
    restoreTimelinePinchPlayhead();
  }

  function markTimelineZooming() {
    isTimelineZooming = true;
    if (timelineZoomingTimer) {
      clearTimeout(timelineZoomingTimer);
    }
    timelineZoomingTimer = setTimeout(() => {
      isTimelineZooming = false;
      timelineZoomingTimer = null;
    }, TIMELINE_ZOOMING_MS);
  }

  function stopTimelineZoomAnimation() {
    if (timelineZoomAnimationFrame !== null && browser) {
      cancelAnimationFrame(timelineZoomAnimationFrame);
    }
    timelineZoomAnimationFrame = null;
    timelineZoomTargetViewport = null;
  }

  function startTimelineDrag(event: PointerEvent, target?: TimelineMarker) {
    if (!inferredTimelineDurationMs() && !syncVideoMetadata()) {
      return;
    }

    event.preventDefault();
    if (event.pointerType === 'touch') {
      updateTimelineTouchPointer(event);
      timelineDragCaptureElement = event.currentTarget instanceof HTMLElement ? event.currentTarget : timelineDragCaptureElement;
      timelineDragCaptureElement?.setPointerCapture?.(event.pointerId);
      if (timelineTouchPointers.size >= 2 && beginTimelinePinch()) {
        return;
      }
    }

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
    if (event.pointerType === 'touch' && timelineDragTarget === 'playhead' && timelineTouchPointers.size === 1) {
      timelinePinchRestorePlayheadMs = playerCurrentMs;
      timelinePinchRestoreLoopSuppression = isLoopSuppressedByTimelineSeek;
    } else if (event.pointerType !== 'touch') {
      timelinePinchRestorePlayheadMs = null;
      timelinePinchRestoreLoopSuppression = null;
    }
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
    if (event.pointerType === 'touch' && timelinePinchActive) {
      event.preventDefault();
      if (timelineTouchPointers.has(event.pointerId)) {
        updateTimelineTouchPointer(event);
        updateTimelinePinch();
      } else {
        restoreTimelinePinchPlayhead();
      }
      return;
    }

    if (event.pointerType === 'touch' && timelineTouchPointers.has(event.pointerId)) {
      updateTimelineTouchPointer(event);
    }

    if (!timelineDragTarget) {
      return;
    }

    setDraftBoundary(timelineDragTarget, timelineMsFromPointer(event));
  }

  function stopTimelineDrag(event?: PointerEvent) {
    if (event?.pointerType === 'touch') {
      timelineTouchPointers.delete(event.pointerId);
      if (timelinePinchActive) {
        if (timelineTouchPointers.size < 2) {
          resetTimelinePinch();
        }
        return;
      }
    }

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

  function setTimelineViewport(nextViewport: TimelineViewport) {
    const timelineDurationMs = inferredTimelineDurationMs();
    const next = clampTimelineViewport({
      startMs: nextViewport.startMs,
      endMs: nextViewport.endMs,
      durationMs: timelineDurationMs
    });

    timelineViewportStartMs = next.startMs;
    timelineViewportEndMs = next.endMs;
    hasManualTimelineZoom = timelineZoomed({
      startMs: timelineViewportStartMs,
      endMs: timelineViewportEndMs,
      durationMs: timelineDurationMs
    });
    if (!hasManualTimelineZoom) {
      isZoomLooping = false;
      isLoopSuppressedByTimelineSeek = false;
    }
  }

  function zoomTimelineAround(anchorMs: number, scale: number, viewport: TimelineViewport | null = null) {
    const timelineDurationMs = inferredTimelineDurationMs();
    if (!timelineDurationMs) {
      return;
    }

    markTimelineZooming();
    ensureTimelineViewport();
    const sourceViewport = viewport ?? {
      startMs: timelineViewportStartMs,
      endMs: timelineViewportEndMs
    };
    const next = zoomTimelineViewport({
      viewportStartMs: sourceViewport.startMs,
      viewportEndMs: sourceViewport.endMs,
      durationMs: timelineDurationMs,
      anchorMs,
      scale
    });
    setTimelineViewport(next);
  }

  function setTimelineZoomTarget(anchorMs: number, scale: number) {
    const timelineDurationMs = inferredTimelineDurationMs();
    if (!timelineDurationMs) {
      return;
    }

    ensureTimelineViewport();
    const sourceViewport = timelineZoomTargetViewport ?? {
      startMs: timelineViewportStartMs,
      endMs: timelineViewportEndMs
    };
    timelineZoomTargetViewport = zoomTimelineViewport({
      viewportStartMs: sourceViewport.startMs,
      viewportEndMs: sourceViewport.endMs,
      durationMs: timelineDurationMs,
      anchorMs,
      scale
    });
    startTimelineZoomAnimation();
  }

  function startTimelineZoomAnimation() {
    if (!browser || timelineZoomAnimationFrame !== null) {
      return;
    }

    markTimelineZooming();
    timelineZoomAnimationFrame = requestAnimationFrame(animateTimelineZoom);
  }

  function animateTimelineZoom() {
    timelineZoomAnimationFrame = null;
    if (!timelineZoomTargetViewport) {
      return;
    }

    const nextStart =
      timelineViewportStartMs + (timelineZoomTargetViewport.startMs - timelineViewportStartMs) * TIMELINE_SMOOTH_ZOOM_EASE;
    const nextEnd =
      timelineViewportEndMs + (timelineZoomTargetViewport.endMs - timelineViewportEndMs) * TIMELINE_SMOOTH_ZOOM_EASE;
    const settled =
      Math.abs(nextStart - timelineZoomTargetViewport.startMs) <= TIMELINE_SMOOTH_ZOOM_SETTLE_MS &&
      Math.abs(nextEnd - timelineZoomTargetViewport.endMs) <= TIMELINE_SMOOTH_ZOOM_SETTLE_MS;

    if (settled) {
      setTimelineViewport(timelineZoomTargetViewport);
      timelineZoomTargetViewport = null;
      return;
    }

    setTimelineViewport({ startMs: nextStart, endMs: nextEnd });
    timelineZoomAnimationFrame = requestAnimationFrame(animateTimelineZoom);
  }

  function handleTimelineWheel(event: WheelEvent) {
    const timelineDurationMs = inferredTimelineDurationMs();
    if (!timelineElement || !timelineDurationMs) {
      return;
    }

    event.preventDefault();
    const boundedDelta = Math.max(-120, Math.min(120, event.deltaY));
    const scale = Math.exp(boundedDelta * TIMELINE_WHEEL_ZOOM_DELTA_SCALE);
    markTimelineZooming();
    setTimelineZoomTarget(timelineMsFromClientX(event.clientX), scale);
  }

  function resetTimelineZoom() {
    stopTimelineZoomAnimation();
    timelineViewportStartMs = 0;
    timelineViewportEndMs = inferredTimelineDurationMs();
    hasManualTimelineZoom = false;
    isZoomLooping = false;
    isLoopSuppressedByTimelineSeek = false;
  }

  function createDraftClipId() {
    if (typeof globalThis.crypto?.randomUUID === 'function') {
      return globalThis.crypto.randomUUID();
    }

    return `draft-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
  }

  function startNewMoveClip() {
    if (!confirmDiscardDraftMoveChanges()) {
      return;
    }

    const actionStart = clampMs(playerCurrentMs || 0);
    const actionEnd = Math.min(
      playerDurationMs || actionStart + DEFAULT_MOVE_DURATION_MS,
      actionStart + DEFAULT_MOVE_DURATION_MS
    );
    enableMoveEditorAudio();
    resetTimelinePromotion();
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
    isZoomLooping = false;
    isLoopSuppressedByTimelineSeek = false;
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

  function openMoveEditor() {
    if (!confirmDiscardDraftMoveChanges()) {
      return;
    }

    resetTimelinePromotion();
    activeClipId = null;
    isDraftingMove = true;
    draftMoveRows = [];
    activeDraftMoveRowId = null;
    isLooping = false;
    isLoopingWithPadding = true;
    isLoopSuppressedByTimelineSeek = false;
    countMode = 'idle';
    countModeIndex = 0;
    isCroppingClip = false;
    cropDragStart = null;
    lastDraftBoundaryTarget = null;
    resumePlaybackAfterTimelineDrag = false;
    draftInitialSnapshot = JSON.stringify([[], draftStartMs, draftEndMs]);
  }

  function exitDraftMove() {
    if (!confirmDiscardDraftMoveChanges()) {
      return;
    }

    resetTimelinePromotion();
    activeClipId = null;
    isDraftingMove = false;
    isLooping = false;
    isLoopingWithPadding = true;
    isLoopSuppressedByTimelineSeek = false;
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

  function handleBeforeUnload(event: BeforeUnloadEvent) {
    if (!hasDraftChanges) {
      return;
    }

    event.preventDefault();
    event.returnValue = '';
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
          actionOutputFilePath: reusableClip?.actionOutputFilePath ?? null,
          lowResOutputFilePath: reusableClip?.lowResOutputFilePath ?? null,
          lowResPaddedOutputFilePath: reusableClip?.lowResPaddedOutputFilePath ?? null,
          publishedAssetId: reusableClip?.publishedAssetId ?? null,
          publishedActionOutputFilePath: reusableClip?.publishedActionOutputFilePath ?? null,
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
    resetTimelinePromotion();
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
      openMoveEditor();
      return;
    }

    if (!activeClipId || !draftMoveRows.length) {
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
    statuses: Array<{
      clipId: string;
      status: DerivedClip['status'];
      error: string | null;
      outputAssetId: string | null;
      actionOutputFilePath?: string | null;
      lowResOutputFilePath?: string | null;
      lowResPaddedOutputFilePath?: string | null;
      publishedActionOutputFilePath?: string | null;
      publishedLowResFilePath?: string | null;
      publishedLowResPaddedFilePath?: string | null;
    }>
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
        outputAssetId: status.outputAssetId,
        actionOutputFilePath: status.actionOutputFilePath ?? clip.actionOutputFilePath,
        lowResOutputFilePath: status.lowResOutputFilePath ?? clip.lowResOutputFilePath,
        lowResPaddedOutputFilePath: status.lowResPaddedOutputFilePath ?? clip.lowResPaddedOutputFilePath,
        publishedActionOutputFilePath: status.publishedActionOutputFilePath ?? clip.publishedActionOutputFilePath,
        publishedLowResFilePath: status.publishedLowResFilePath ?? clip.publishedLowResFilePath,
        publishedLowResPaddedFilePath: status.publishedLowResPaddedFilePath ?? clip.publishedLowResPaddedFilePath
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
    clearTimelinePromotionTimers();
    stopTimelineZoomAnimation();
    if (timelineZoomingTimer) {
      clearTimeout(timelineZoomingTimer);
    }
    stopPolling();
    stopPlaybackAnimation();
  });

  onMount(() => {
    if (!browser) {
      return;
    }

    const reducedMotionQuery = window.matchMedia('(prefers-reduced-motion: reduce)');
    const syncReducedMotion = () => {
      prefersReducedMotion = reducedMotionQuery.matches;
    };

    syncReducedMotion();
    reducedMotionQuery.addEventListener('change', syncReducedMotion);
    document.addEventListener('fullscreenchange', syncVideoFullscreenState);
    document.addEventListener('webkitfullscreenchange', syncVideoFullscreenState);
    return () => {
      reducedMotionQuery.removeEventListener('change', syncReducedMotion);
      document.removeEventListener('fullscreenchange', syncVideoFullscreenState);
      document.removeEventListener('webkitfullscreenchange', syncVideoFullscreenState);
    };
  });
</script>

<svelte:head>
  <title>Media | Salsa Encyclopedia</title>
</svelte:head>

<svelte:window
  on:pointermove={handleTimelinePointerMove}
  on:pointerup={(event) => stopTimelineDrag(event)}
  on:pointercancel={(event) => stopTimelineDrag(event)}
  on:keydown={handleKeydown}
  on:beforeunload={handleBeforeUnload}
/>

<div class="stack upload-page media-page media-editor-page">
  <section class="panel upload-shell">
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

              <div
                class:expanded={isDraftingMove}
                class:dragging={Boolean(timelineDragTarget)}
                class:zooming={isTimelineZooming}
                class="timeline-card"
              >
                <div class="timeline-meta">
                  {#if activeDraftMoveRow}
                    <span><strong>Clip</strong> {formatSeconds(draftStartMs)}s - {formatSeconds(draftEndMs)}s</span>
                    <span><strong>Move</strong> {formatSeconds(draftActionStartMs)}s - {formatSeconds(draftActionEndMs)}s</span>
                    <span><strong>Length</strong> {formatSeconds(Math.max(0, draftEndMs - draftStartMs))}s</span>
                  {/if}
                  <span class="editor-video-controls" class:marker-dragging={isDraggingTimelineMarker} on:click={(event) => event.stopPropagation()}>
                    <button
                      class="editor-video-play"
                      class:playing={isPlaying}
                      type="button"
                      aria-label={isPlaying ? 'Pause source video' : 'Play source video'}
                      disabled={isDraggingTimelineMarker}
                      aria-hidden={isDraggingTimelineMarker}
                      tabindex={isDraggingTimelineMarker ? -1 : undefined}
                      on:click={() => void togglePlayback()}
                    >
                      <span>{isPlaying ? 'Pause' : 'Play'}</span>
                    </button>
                    <button
                      class="editor-video-fullscreen"
                      class:active={isVideoFullscreen}
                      type="button"
                      aria-label={isVideoFullscreen ? 'Exit source video fullscreen' : 'View source video fullscreen'}
                      aria-pressed={isVideoFullscreen}
                      title={isVideoFullscreen ? 'Exit fullscreen' : 'Fullscreen'}
                      on:click={() => void toggleVideoFullscreen()}
                    >
                      <span>{isVideoFullscreen ? 'Exit fullscreen' : 'Fullscreen'}</span>
                    </button>
                    <span class="editor-video-volume-wrap" class:open={isVolumeOpen}>
                      <button
                        class="editor-video-volume-toggle"
                        class:muted={isMuted || videoVolume === 0}
                        type="button"
                        aria-label={isMuted || videoVolume === 0 ? 'Unmute source video' : 'Mute source video'}
                        aria-pressed={isMuted || videoVolume === 0}
                        on:click={toggleMuted}
                      >
                        <span>{isMuted || videoVolume === 0 ? 'Unmute' : 'Mute'}</span>
                      </button>
                      <button
                        class="editor-video-volume-panel-toggle"
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
                  <span
                    class="timeline-zoom-status"
                    class:visible={timelineZoomControlsVisible}
                    aria-hidden={!timelineZoomControlsVisible}
                  >
                    <span class="timeline-zoom-pill" class:timeline-zoom-active={timelineIsZoomed}>
                      <strong>Zoomed</strong>
                      {formatRoundedSeconds(timelineViewportDurationMs())}s
                      ({formatRoundedSeconds(timelineViewportStartMs)}s - {formatRoundedSeconds(timelineViewportEndMs)}s)
                    </span>
                    <span class="timeline-zoom-actions">
                      {#each timelineZoomButtons as action (action.key)}
                        <button
                          class={`timeline-move-action${action.className ? ` ${action.className}` : ''}`}
                          class:active={action.active}
                          type="button"
                          aria-label={action.ariaLabel}
                          aria-pressed={action.ariaPressed}
                          title={action.title}
                          disabled={!timelineZoomControlsVisible}
                          tabindex={timelineZoomControlsVisible ? undefined : -1}
                          animate:flip={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicOut }}
                          on:click={action.onClick}
                        >
                          {action.label}
                        </button>
                      {/each}
                    </span>
                  </span>
                  {#if isDraftingMove}
                    {#if activeDraftMoveRow}
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
                      </span>
                    {/if}
                  {/if}
                </div>
                {#if showPlaybackMoveContext}
                  <div
                    class="timeline-now-playing"
                    class:timeline-edit-context={isDraftingMove}
                    aria-label={isDraftingMove ? 'Move edit context' : 'Current move context'}
                  >
                    <div class="timeline-context-slot timeline-context-slot-previous">
                      {#if timelineMoveContext.previous}
                        {#if isDraftingMove && timelineMoveContext.previous.row}
                          <button
                            type="button"
                            class="timeline-context-box timeline-context-side timeline-context-previous timeline-context-button"
                            aria-label={timelineContextActionLabel('previous', timelineMoveContext.previous)}
                            on:click={() => openEditorContextRow(timelineMoveContext.previous?.row ?? null)}
                            in:fly={{ x: -6, duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicInOut }}
                            out:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                          >
                            <strong>{timelineMoveContext.previous.label}</strong>
                          </button>
                        {:else}
                          <div
                            class="timeline-context-box timeline-context-side timeline-context-previous"
                            in:fly={{ x: -6, duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicInOut }}
                            out:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                          >
                            <strong>{timelineMoveContext.previous.label}</strong>
                          </div>
                        {/if}
                      {/if}
                    </div>
                    <div
                      class="timeline-context-box timeline-context-current"
                      class:empty={Boolean(timelineMoveContext.current?.empty)}
                    >
                      {#key timelineMoveContext.current?.key ?? 'empty'}
                        <strong>{timelineMoveContext.current?.label ?? '—'}</strong>
                      {/key}
                    </div>
                    <div class="timeline-context-slot timeline-context-slot-next">
                      {#if timelineMoveContext.next}
                        {#if isDraftingMove && timelineMoveContext.next.row}
                          <button
                            type="button"
                            class="timeline-context-box timeline-context-side timeline-context-next timeline-context-button"
                            aria-label={timelineContextActionLabel('next', timelineMoveContext.next)}
                            on:click={() => openEditorContextRow(timelineMoveContext.next?.row ?? null)}
                            in:fly={{ x: 6, duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicInOut }}
                            out:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                          >
                            <strong>{timelineMoveContext.next.label}</strong>
                          </button>
                        {:else}
                          <div
                            class="timeline-context-box timeline-context-side timeline-context-next"
                            in:fly={{ x: 6, duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicInOut }}
                            out:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                          >
                            <strong>{timelineMoveContext.next.label}</strong>
                          </div>
                        {/if}
                      {/if}
                    </div>
                  </div>
                {/if}
                <div class="timeline-overview" class:zoomed={timelineIsZoomed} aria-hidden="true">
                  <span class="timeline-overview-track">
                    <span
                      class="timeline-overview-window"
                      style={`left: ${timelineOverviewLeft(timelineScaleKey)}%; width: ${timelineOverviewWidth(timelineScaleKey)}%`}
                    ></span>
                  </span>
                </div>
                <div class="clip-timeline-shell" class:zoomed={timelineIsZoomed}>
                  <div
                    class:zoomed={timelineIsZoomed}
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
                      class:current={currentPlaybackMove?.id === clip.id}
                      style={savedClipRangeStyle(clip, timelineScaleKey)}
                      in:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                      out:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                      aria-label={`${clipDisplayName(clip)} ${clipChangeLabel(clipChangeStates.get(clip.id)).toLowerCase()} clip`}
                      title={`${clipDisplayName(clip)} · ${clipChangeLabel(clipChangeStates.get(clip.id))}`}
                      on:pointerdown={(event) => event.stopPropagation()}
                      on:click={(event) => {
                        event.stopPropagation();
                        openSavedClipEditor(clip);
                      }}
                    >
                      {#if savedClipUsesDraftMove(clip)}
                        <span class="timeline-draft-label" aria-hidden="true">draft</span>
                      {/if}
                    </button>
                  {/each}
                  {#if isDraftingMove && activeDraftMoveRow}
                    {#if timelineEditorChromeVisible}
                      <div
                        class="clip-timeline-selection clip-range"
                        style={timelineRangeStyle(draftStartMs, draftEndMs, timelineScaleKey)}
                        in:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                      ></div>
                    {/if}
                    {#each visibleEditorDraftMoveRows as row (row.id)}
                      <div
                        class="clip-timeline-selection move-range"
                        class:active={row.id === activeDraftMoveRowId}
                        class:secondary={row.id !== activeDraftMoveRowId}
                        class:promoting-from-saved={row.id === timelinePromotingDraftRowId && timelinePromotionAtSavedLane}
                        style={draftMoveRangeStyle(row, timelineScaleKey)}
                        on:dblclick={(event) => editDraftMoveRowFromTimeline(event, row.id)}
                      >
                        {#if draftRowUsesDraftMove(row)}
                          <span class="timeline-draft-label" aria-hidden="true">draft</span>
                        {/if}
                      </div>
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
                  {#if isDraftingMove && activeDraftMoveRow && timelineEditorChromeVisible}
                    <button
                      type="button"
                      class="clip-timeline-marker clip-marker"
                      style={markerLeftStyle(draftStartMs, timelineScaleKey)}
                      aria-label="Drag clip start"
                      title="Clip starts"
                      in:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
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
                      in:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
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
                      in:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
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
                      in:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                      on:pointerdown={(event) => (event.stopPropagation(), startTimelineDrag(event, 'moveEnd'))}
                    >
                      <span class="clip-timeline-marker-head" aria-hidden="true">
                        <span class="clip-timeline-marker-fill"></span>
                      </span>
                      <span class="clip-timeline-marker-stem" aria-hidden="true"></span>
                    </button>
                  {/if}
                  </div>
                </div>
                <div class="timeline-move-actions">
                  {#each timelineActionButtons as action (action.key)}
                    <button
                      class={`timeline-move-action${action.className ? ` ${action.className}` : ''}`}
                      class:active={action.active}
                      type="button"
                      aria-label={action.ariaLabel}
                      aria-pressed={action.ariaPressed}
                      title={action.title}
                      animate:flip={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicOut }}
                      in:fly={{ y: -4, duration: motionDuration(MEDIA_MOTION_SHORT_MS), easing: cubicOut }}
                      out:fade={{ duration: motionDuration(MEDIA_MOTION_SHORT_MS) }}
                      on:click={action.onClick}
                    >
                      {action.label}
                    </button>
                  {/each}
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
                    <div class="draft-move-row-window" bind:this={draftMoveRowWindowElement}>
                      {#each visibleEditorMoveRows as item (item.key)}
                        <div
                          class="draft-move-row"
                          class:active={item.kind === 'draft' && item.row.id === activeDraftMoveRowId}
                          class:bound={item.kind === 'draft' ? Boolean(item.row.timingGroupId) : Boolean(item.clip.timingGroupId)}
                          class:saved-editor-row={item.kind === 'saved'}
                          data-draft-row-id={item.kind === 'draft' ? item.row.id : undefined}
                          data-clip-row-id={item.kind === 'draft' ? item.row.originalClipId ?? undefined : item.clip.id}
                        >
                          {#if item.kind === 'draft'}
                            {@const row = item.row}
                            {@const sourceClip = row.originalClipId ? clipRows.find((clip) => clip.id === row.originalClipId) : null}
                            <div class="move-start-display">
                              <strong>{formatTenthSeconds(row.startMs)}s</strong>
                            </div>
                            <div class="move-link-field">
                              <EntityPicker
                                template={draftMovePickerTemplate}
                                options={availableMoves}
                                selectedIds={row.moveIds}
                                excludedIds={selectedDraftMoveIds}
                                query={row.query}
                                limit={MOVE_SUGGESTION_LIMIT}
                                onfocus={() => selectDraftMoveRow(row.id)}
                                onquery={(detail) => handleDraftMoveQueryInput(row.id, detail.query)}
                                oncreate={(detail) => createDraftMoveFromQuery(row.id, detail.value)}
                                onselect={(detail) => addDraftMove(detail.id, row.id)}
                                onremove={(detail) => removeDraftMove(row.id, detail.id)}
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
                              <EntityPicker
                                template={startPositionPickerTemplate}
                                options={positionPickerOptions}
                                selectedIds={row.startPositionId ? [row.startPositionId] : []}
                                query={row.startPositionQuery}
                                limit={MOVE_SUGGESTION_LIMIT}
                                onfocus={() => selectDraftMoveRow(row.id)}
                                onquery={(detail) => updateDraftPositionQuery(row.id, 'start', detail.query)}
                                onselect={(detail) => selectDraftPosition(row.id, 'start', detail.id)}
                                oncreate={(detail) => void createDraftPositionFromQuery(row.id, 'start', detail.value)}
                                onremove={() => removeDraftPosition(row.id, 'start')}
                              />
                            </div>
                            <div class="draft-position-field">
                              <EntityPicker
                                template={endPositionPickerTemplate}
                                options={positionPickerOptions}
                                selectedIds={row.endPositionId ? [row.endPositionId] : []}
                                query={row.endPositionQuery}
                                limit={MOVE_SUGGESTION_LIMIT}
                                onfocus={() => selectDraftMoveRow(row.id)}
                                onquery={(detail) => updateDraftPositionQuery(row.id, 'end', detail.query)}
                                onselect={(detail) => selectDraftPosition(row.id, 'end', detail.id)}
                                oncreate={(detail) => void createDraftPositionFromQuery(row.id, 'end', detail.value)}
                                onremove={() => removeDraftPosition(row.id, 'end')}
                              />
                            </div>
                            {#if sourceClip}
                              <button
                                class="draft-edit-button key-video-star"
                                type="button"
                                aria-pressed={sourceClip.isKeyVideo}
                                title={sourceClip.isKeyVideo ? 'Remove key video' : 'Make key video'}
                                on:click={() => toggleClipKeyVideo(sourceClip.id)}
                              >
                                {sourceClip.isKeyVideo ? '★' : '☆'}
                              </button>
                            {/if}
                            {#if row.id === activeDraftMoveRowId}
                              <button class="draft-edit-button active" type="button" disabled aria-pressed="true">Editing</button>
                            {:else}
                              <button class="draft-edit-button" type="button" on:click={() => selectDraftMoveRow(row.id)}>Edit</button>
                            {/if}
                          {:else}
                            {@const clip = item.clip}
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
                          {/if}
                        </div>
                      {/each}
                    </div>
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
                    <EntityPicker
                      template={mediaDancerPickerTemplate}
                      options={dancerOptions}
                      selectedIds={editDancerIds}
                      query={editDancerQuery}
                      onquery={(detail) => (editDancerQuery = detail.query)}
                      onselect={(detail) => addSelectedDancer(detail.value)}
                      onremove={(detail) => removeSelectedDancer(detail.value)}
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
