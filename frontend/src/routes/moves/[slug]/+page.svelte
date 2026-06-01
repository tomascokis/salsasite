<script lang="ts">
  import { browser } from '$app/environment';
  import { invalidateAll } from '$app/navigation';
  import { moveDisplayId } from '$lib/move-id';
  import { applyVideoAudioPreference, saveVideoAudioPreferenceFromElement } from '$lib/video-audio-preference';
  import { onDestroy } from 'svelte';
  import AutoResizeTextarea from '$lib/components/AutoResizeTextarea.svelte';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import MoveRelationshipDiagram from '$lib/components/MoveRelationshipDiagram.svelte';
  import MoveTypeControl from '$lib/components/MoveTypeControl.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import type {
    MetadataEntry,
    MoveVideoEntry,
    MoveRecord,
    RawMoveReferenceRecord,
    RelationshipDiagram,
    SiteMetadata
  } from '$lib/types';

  type MoveOption = {
    id: string;
    slug: string;
    name: string | null;
    positions?: string | null;
    tags?: string | null;
    source?: string | null;
  };

  export let data: {
    move: MoveRecord;
    rawReference: RawMoveReferenceRecord | null;
    relationshipDiagram: RelationshipDiagram;
    metadata: SiteMetadata;
    moves: MoveOption[];
    videos: MoveVideoEntry[];
  };

  let videos = data.videos;
  let selectedVideo = 0;
  let selectedVideoVariant: 'full' | 'low' | 'padded-low' = 'full';
  let videoElement: HTMLVideoElement | null = null;
  let posterPollTimeout: ReturnType<typeof setTimeout> | null = null;
  let posterPollTarget: string | null = null;
  let selectedVideoEntry: MoveVideoEntry | null = null;
  let showCountOverlay = false;
  let currentVideoMs = 0;
  let move = data.move;
  let isEditingMoveDetails = false;
  let name = '';
  let topic = '';
  let level = '';
  let moveType = '';
  let group = '';
  let positions = '';
  let tags = '';
  let source = '';
  let description = '';
  let comments = '';
  let topicQuery = '';
  let familyQuery = '';
  let positionsQuery = '';
  let tagsQuery = '';
  let sourceQuery = '';
  let detailEditorStatus = '';
  let isSavingMoveDetails = false;
  let isSavingKeyVideo = false;
  const unavailablePosterVideos = new Set<string>();
  const levelOptions = ['', '1', '2', '3', '4', '5'];
  $: family = move.group?.trim() || data.rawReference?.family?.trim() || null;

  function normalizeSubtitleValue(value: string) {
    return value
      .toLocaleLowerCase()
      .replace(/[^\p{L}\p{N}]+/gu, ' ')
      .trim()
      .split(/\s+/)
      .map((token) => (token.length > 3 && token.endsWith('s') ? token.slice(0, -1) : token))
      .join(' ');
  }

  $: subtitleParts = [move.topic?.trim() || null, family].filter((value, index, values) => {
    if (!value) {
      return false;
    }

    const normalized = normalizeSubtitleValue(value);
    return (
      values.findIndex(
        (candidate) => candidate && normalizeSubtitleValue(candidate) === normalized
      ) === index
    );
  });
  $: subtitle = subtitleParts.join(' • ');

  $: visibleMeta = [
    ['ID', moveDisplayId(move)],
    ['Level', move.level],
    ['Type', move.type],
    ['Group', move.group],
    ['Positions', move.positions],
    ['See also', move.seeAlso],
    ['Tags', move.tags],
    ['Source', move.source]
  ].filter(([, value]) => value);

  $: movePropertyBadges = [
    move.level ? `Level ${move.level}` : null,
    move.type,
    move.group,
    move.positions
  ].filter((value): value is string => Boolean(value));

  const relationshipCard = data.relationshipDiagram.meta.hasDiagram;
  const relationshipWide = data.relationshipDiagram.meta.isLarge;
  const MAX_MAIN_MOVE_VIDEOS = 4;
  $: topicOptions = metadataPickerOptions(data.metadata.topics, topic, 'topic');
  $: familyOptions = metadataPickerOptions(data.metadata.families, group, 'family');
  $: positionOptions = uniqueMoveTextOptions(data.moves.map((entry) => entry.positions));
  $: sourceOptions = uniqueMoveTextOptions(data.moves.map((entry) => entry.source));
  $: tagOptions = uniqueMoveTextOptions(data.moves.flatMap((entry) => splitTagText(entry.tags ?? '')));
  $: selectedTopicIds = metadataSelectionIds(data.metadata.topics, topic, 'topic');
  $: selectedFamilyIds = metadataSelectionIds(data.metadata.families, group, 'family');
  $: selectedPositionIds = positions ? [positions] : [];
  $: selectedSourceIds = source ? [source] : [];
  $: selectedTagIds = splitTagText(tags);

  resetDetailEditorFromMove(move);

  function metadataOption(entry: MetadataEntry) {
    return {
      id: entry.id,
      label: entry.name,
      secondary: `${entry.moveCount} moves`
    };
  }

  function metadataCustomId(kind: 'topic' | 'family', value: string) {
    return `custom-${kind}:${value.trim().toLocaleLowerCase()}`;
  }

  function metadataSelectionIds(entries: MetadataEntry[], value: string, kind: 'topic' | 'family') {
    const normalized = value.trim().toLocaleLowerCase();
    if (!normalized) return [];
    const existing = entries.find((entry) => entry.name.trim().toLocaleLowerCase() === normalized);
    return [existing?.id ?? metadataCustomId(kind, value)];
  }

  function matchingMetadataIds(entries: MetadataEntry[], value: string) {
    const normalized = value.trim().toLocaleLowerCase();
    return entries.filter((entry) => entry.name.trim().toLocaleLowerCase() === normalized).map((entry) => entry.id);
  }

  function metadataPickerOptions(entries: MetadataEntry[], value: string, kind: 'topic' | 'family') {
    const options = entries.map(metadataOption);
    const customValue = value.trim();
    if (!customValue || matchingMetadataIds(entries, customValue).length) {
      return options;
    }
    return [
      {
        id: metadataCustomId(kind, customValue),
        label: customValue,
        secondary: 'New'
      },
      ...options
    ];
  }

  function uniqueMoveTextOptions(values: Array<string | null | undefined>) {
    return Array.from(new Set(values.map((value) => String(value ?? '').trim()).filter(Boolean)))
      .sort((left, right) => left.localeCompare(right))
      .map((value) => ({ id: value, label: value }));
  }

  function splitTagText(value: string | null | undefined) {
    return String(value ?? '')
      .split(/[,;]+/)
      .map((entry) => entry.trim())
      .filter(Boolean);
  }

  function addTagValue(value: string) {
    const next = value.trim();
    if (!next) return;
    const existing = splitTagText(tags);
    if (!existing.some((entry) => entry.toLocaleLowerCase() === next.toLocaleLowerCase())) {
      tags = [...existing, next].join(', ');
    }
    tagsQuery = '';
  }

  function removeTagValue(value: string) {
    tags = splitTagText(tags)
      .filter((entry) => entry !== value)
      .join(', ');
  }

  async function persistMetadata(kind: 'topic' | 'family', value: string) {
    const name = value.trim();
    if (!name) return false;
    const response = await fetch('/api/metadata', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ kind, name })
    });
    if (!response.ok) {
      const payload = await response.json().catch(() => ({}));
      detailEditorStatus = payload.error ?? `Could not save ${kind}.`;
      return false;
    }
    await invalidateAll();
    return true;
  }

  function resetDetailEditorFromMove(sourceMove: MoveRecord) {
    name = sourceMove.name ?? '';
    topic = sourceMove.topic ?? '';
    level = sourceMove.level ?? '';
    moveType = sourceMove.type ?? '';
    group = sourceMove.group ?? '';
    positions = sourceMove.positions ?? '';
    tags = sourceMove.tags ?? '';
    source = sourceMove.source ?? '';
    description = sourceMove.description ?? '';
    comments = sourceMove.comments ?? '';
    topicQuery = '';
    familyQuery = '';
    positionsQuery = '';
    tagsQuery = '';
    sourceQuery = '';
  }

  function startMoveDetailsEditor() {
    resetDetailEditorFromMove(move);
    detailEditorStatus = '';
    isEditingMoveDetails = true;
  }

  function cancelMoveDetailsEditor() {
    resetDetailEditorFromMove(move);
    detailEditorStatus = '';
    isEditingMoveDetails = false;
  }

  async function saveMoveDetails() {
    isSavingMoveDetails = true;
    detailEditorStatus = 'Saving...';

    const response = await fetch(`/api/moves/${encodeURIComponent(move.id)}`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        name,
        topic,
        level,
        type: moveType,
        category: move.category,
        group,
        baseMove: move.baseMove,
        positions,
        seeAlso: move.seeAlso,
        tags,
        source,
        description,
        comments,
        reviewFlag: Boolean(move.reviewFlag),
        reviewNotes: move.reviewNotes ?? '',
        parentIds: move.parentIds,
        childIds: move.childIds,
        relatedMoveIds: move.relatedMoveIds
      })
    });
    const payload = await response.json();
    isSavingMoveDetails = false;

    if (!response.ok) {
      detailEditorStatus = payload.error ?? 'Could not save move.';
      return;
    }

    move = payload.move;
    resetDetailEditorFromMove(move);
    detailEditorStatus = 'Saved.';
    isEditingMoveDetails = false;
  }

  function mediaUrl(file: string) {
    return `/media/${encodeURIComponent(file)}`;
  }

  function visibleVideoLayout(entries: MoveVideoEntry[]) {
    if (entries.length <= MAX_MAIN_MOVE_VIDEOS) {
      return {
        main: entries.map((entry, index) => ({ entry, index })),
        overflow: [] as Array<{ entry: MoveVideoEntry; index: number }>
      };
    }

    const mainIndexes: number[] = [];
    entries.forEach((entry, index) => {
      if (entry.isKeyVideo && mainIndexes.length < MAX_MAIN_MOVE_VIDEOS) {
        mainIndexes.push(index);
      }
    });

    entries.forEach((_, index) => {
      if (mainIndexes.length >= MAX_MAIN_MOVE_VIDEOS || mainIndexes.includes(index)) {
        return;
      }
      mainIndexes.push(index);
    });

    return {
      main: mainIndexes.map((index) => ({ entry: entries[index], index })),
      overflow: entries
        .map((entry, index) => ({ entry, index }))
        .filter(({ index }) => !mainIndexes.includes(index))
    };
  }

  function clipEditUrl(entry: MoveVideoEntry) {
    if (!entry.sourceAssetId || !entry.clipId) {
      return null;
    }

    return `/media/edit/${encodeURIComponent(entry.sourceAssetId)}?clip=${encodeURIComponent(entry.clipId)}&mode=clip`;
  }

  function videoDancers(entry: MoveVideoEntry) {
    const dancers = entry.sourceDancers.length ? entry.sourceDancers : entry.dancers;
    return dancers.length ? dancers.join(', ') : null;
  }

  function videoOriginSummary(entry: MoveVideoEntry) {
    if (entry.isDerived) {
      return `Derived from ${entry.sourceDisplayName ?? 'source video'}`;
    }

    return entry.kind === 'source' ? 'Full source video' : 'Standalone move video';
  }

  function videoTabLabel(entry: MoveVideoEntry) {
    const base = [entry.timingLabel, entry.contentTypeLabel, entry.environmentLabel].filter(Boolean).join(', ') || entry.displayName;
    return entry.descriptorLabel ? `${base} - ${entry.descriptorLabel}` : base;
  }

  function videoTabContext(entry: MoveVideoEntry) {
    return videoDancers(entry);
  }

  function videoMetaRows(entry: MoveVideoEntry) {
    const rows: Array<[string, string | null]> = [
      ['Dancers', videoDancers(entry)],
      ['Origin', videoOriginSummary(entry)],
      ['Recorded', entry.sourceRecordDate ?? entry.recordDate],
      ['Class', entry.sourceClassWorkshop ?? entry.classWorkshop],
      [
        'Tags',
        entry.sourceTags.length ? entry.sourceTags.join(', ') : entry.tags.length ? entry.tags.join(', ') : null
      ],
      ['Notes', entry.sourceNotes ?? entry.notes]
    ];

    return rows.filter((row): row is [string, string] => Boolean(row[1]));
  }

  function timingBadgeClass(value: MoveVideoEntry['timing']) {
    return `badge-timing-${value}`;
  }

  function contentBadgeClass(value: MoveVideoEntry['contentType']) {
    return `badge-content-${value}`;
  }

  function environmentBadgeClass(value: MoveVideoEntry['environment']) {
    return `badge-environment-${value}`;
  }

  function videoVariantOptions(entry: MoveVideoEntry | null) {
    if (!entry) return [];
    return [
      { id: 'full', label: 'Full', filePath: entry.filePath },
      entry.lowResFilePath ? { id: 'low', label: 'Low-res action', filePath: entry.lowResFilePath } : null,
      entry.lowResPaddedFilePath ? { id: 'padded-low', label: 'Low-res padded', filePath: entry.lowResPaddedFilePath } : null
    ].filter((option): option is { id: 'full' | 'low' | 'padded-low'; label: string; filePath: string } => Boolean(option));
  }

  function selectedVideoFile(entry: MoveVideoEntry | null) {
    if (!entry) return null;
    return videoVariantOptions(entry).find((option) => option.id === selectedVideoVariant)?.filePath ?? entry.filePath;
  }

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }

  function posterStatusUrl(file: string) {
    return `/api/posters/${encodeURIComponent(file)}`;
  }

  function currentPosterUrl() {
    const file = videos[selectedVideo]?.posterFile;
    return file ? posterUrl(file) : undefined;
  }

  function currentVideoFile() {
    return selectedVideoFile(videos[selectedVideo] ?? null);
  }

  function isOverflowVideoSelected() {
    return overflowVideos.some(({ index }) => index === selectedVideo);
  }

  async function toggleSelectedVideoKeyState() {
    if (!selectedVideoEntry?.clipId) {
      return;
    }

    isSavingKeyVideo = true;
    detailEditorStatus = 'Saving...';
    const response = await fetch(`/api/clips/${encodeURIComponent(selectedVideoEntry.clipId)}/key`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ isKeyVideo: !selectedVideoEntry.isKeyVideo })
    });
    const payload = await response.json();
    isSavingKeyVideo = false;

    if (!response.ok) {
      detailEditorStatus = payload.error ?? 'Could not update key video.';
      return;
    }

    videos = videos.map((video) =>
      video.clipId === selectedVideoEntry.clipId ? { ...video, isKeyVideo: payload.clip.isKeyVideo } : video
    );
    detailEditorStatus = payload.clip.isKeyVideo ? 'Key video saved.' : 'Key video removed.';
  }

  function applySavedAudioPreference() {
    if (!browser || !videoElement) {
      return;
    }

    applyVideoAudioPreference(videoElement);
  }

  function handleVolumeChange() {
    if (!browser || !videoElement) {
      return;
    }

    saveVideoAudioPreferenceFromElement(videoElement);
  }

  function handleVideoTimeUpdate() {
    currentVideoMs = Math.round((videoElement?.currentTime ?? 0) * 1000);
  }

  function visibleCountMarker(
    entry: MoveVideoEntry | null,
    currentMs: number,
    enabled: boolean,
    variant: 'full' | 'low' | 'padded-low'
  ) {
    if (!entry || !enabled) {
      return null;
    }

    let visible: MoveVideoEntry['countMarkers'][number] | null = null;
    const sourceMs = currentMs + countMarkerSourceOffset(entry, variant);
    for (const marker of [...entry.countMarkers].sort((left, right) => left.ms - right.ms)) {
      if (marker.ms > sourceMs) {
        break;
      }
      visible = marker.clear ? null : marker;
    }
    return visible;
  }

  function countMarkerSourceOffset(entry: MoveVideoEntry, variant: 'full' | 'low' | 'padded-low') {
    if (variant === 'low') {
      return entry.clipActionStartMs ?? entry.clipStartMs ?? 0;
    }

    return entry.clipStartMs ?? 0;
  }

  function countOverlayClass(entry: MoveVideoEntry) {
    return `count-overlay count-overlay-${entry.countOverlayPlacement}`;
  }

  function setPosterForVideo(videoFile: string, posterFile: string) {
    const index = videos.findIndex((entry) => entry.filePath === videoFile);

    if (index === -1 || videos[index]?.posterFile === posterFile) {
      return;
    }

    videos = videos.map((video, videoIndex) =>
      videoIndex === index ? { ...video, posterFile } : video
    );
  }

  function stopPosterPolling() {
    posterPollTarget = null;

    if (posterPollTimeout) {
      clearTimeout(posterPollTimeout);
      posterPollTimeout = null;
    }
  }

  async function pollPoster(videoFile: string) {
    try {
      const response = await fetch(posterStatusUrl(videoFile));
      if (posterPollTarget !== videoFile) {
        return;
      }

      if (!response.ok) {
        stopPosterPolling();
        return;
      }

      const payload = await response.json();

      if (payload.ready && payload.posterFile) {
        setPosterForVideo(videoFile, payload.posterFile);
        stopPosterPolling();
        return;
      }

      if (payload.unavailable) {
        unavailablePosterVideos.add(videoFile);
        stopPosterPolling();
        return;
      }

      posterPollTimeout = setTimeout(() => {
        void pollPoster(videoFile);
      }, 2000);
    } catch {
      if (posterPollTarget !== videoFile) {
        return;
      }

      posterPollTimeout = setTimeout(() => {
        void pollPoster(videoFile);
      }, 3000);
    }
  }

  function ensurePosterPolling() {
    if (!browser) {
      return;
    }

    const videoFile = currentVideoFile();
    const posterFile = videos[selectedVideo]?.posterFile;
    if (!videoFile || posterFile || unavailablePosterVideos.has(videoFile)) {
      stopPosterPolling();
      return;
    }

    if (posterPollTarget === videoFile) {
      return;
    }

    stopPosterPolling();
    posterPollTarget = videoFile;
    void pollPoster(videoFile);
  }

  $: if (videoElement) {
    applySavedAudioPreference();
  }

  $: selectedVideoEntry = videos[selectedVideo] ?? null;
  $: videoLayout = visibleVideoLayout(videos);
  $: mainTabVideos = videoLayout.main;
  $: overflowVideos = videoLayout.overflow;
  $: selectedVideoVariants = videoVariantOptions(selectedVideoEntry);
  $: if (selectedVideoEntry && !selectedVideoVariants.some((option) => option.id === selectedVideoVariant)) {
    selectedVideoVariant = 'full';
  }
  $: selectedClipEditUrl = selectedVideoEntry ? clipEditUrl(selectedVideoEntry) : null;
  $: selectedCountMarker = visibleCountMarker(selectedVideoEntry, currentVideoMs, showCountOverlay, selectedVideoVariant);

  $: ensurePosterPolling();

  onDestroy(() => {
    stopPosterPolling();
  });
</script>

<svelte:head>
  <title>{move.name ?? moveDisplayId(move)} | Salsa Encyclopedia</title>
</svelte:head>

<div class="move-layout">
  <a class="pill move-backlink" href="/">Back to overview</a>

  <section class="move-header">
    <div class="panel video-panel">
      <div class="panel-header move-titleblock">
        <div>
          <h2>{move.name ?? moveDisplayId(move)}</h2>
          {#if subtitle}
            <p class="muted move-subtitle">{subtitle}</p>
          {/if}
        </div>
      </div>
      <div class="media-stage">
        {#if videos.length}
          <div class="tab-list video-tab-list">
            {#each mainTabVideos as { entry: video, index }}
              <button
                type="button"
                class:active={selectedVideo === index}
                aria-label={`${videoTabLabel(video)} video details`}
                on:click={() => {
                  selectedVideo = index;
                  selectedVideoVariant = 'full';
                  showCountOverlay = false;
                  currentVideoMs = 0;
                }}
              >
                <span class="video-tab-icon" aria-hidden="true">
                  <svg viewBox="0 0 24 24" focusable="false">
                    <circle cx="12" cy="12" r="9"></circle>
                    <path d="M10 8.7L16 12l-6 3.3z"></path>
                  </svg>
                </span>
                <span class="video-tab-text">
                  <span class="video-tab-label">{videoTabLabel(video)}</span>
                  {#if videoTabContext(video)}
                    <span class="video-tab-context">{videoTabContext(video)}</span>
                  {/if}
                </span>
                <span class="video-tab-popover" role="tooltip">
                  {#each videoMetaRows(video) as [label, value]}
                    <span class="video-tab-meta-row" class:notes={label === 'Notes'}>
                      <strong>{label}</strong>
                      <span>{value}</span>
                    </span>
                  {/each}
                </span>
              </button>
            {/each}
          </div>
          {#if overflowVideos.length}
            <div class="video-overflow-picker">
              <label>
                <span>More videos</span>
                <select
                  value={isOverflowVideoSelected() ? String(selectedVideo) : ''}
                  on:change={(event) => {
                    const rawValue = (event.currentTarget as HTMLSelectElement).value;
                    if (rawValue === '') {
                      return;
                    }
                    const nextIndex = Number(rawValue);
                    if (Number.isFinite(nextIndex)) {
                      selectedVideo = nextIndex;
                      selectedVideoVariant = 'full';
                      showCountOverlay = false;
                      currentVideoMs = 0;
                    }
                  }}
                >
                  <option value="">Choose hidden video</option>
                  {#each overflowVideos as { entry: video, index }}
                    <option value={index}>{videoTabLabel(video)}</option>
                  {/each}
                </select>
              </label>
            </div>
          {/if}
          {#key `${currentVideoFile() ?? ''}::${currentPosterUrl() ?? ''}`}
            <div class="video-frame">
              <video
                bind:this={videoElement}
                controls
                on:volumechange={handleVolumeChange}
                on:timeupdate={handleVideoTimeUpdate}
                preload="metadata"
                poster={currentPosterUrl()}
                src={mediaUrl(currentVideoFile() ?? videos[selectedVideo].filePath)}
              ></video>
              {#if selectedVideoEntry && selectedCountMarker}
                <span class={countOverlayClass(selectedVideoEntry)}>
                  {selectedCountMarker.count}
                </span>
              {/if}
            </div>
          {/key}

          {#if selectedVideoEntry}
            <div class="video-actions">
              <span class="content-badge-list video-state-badges">
                <ContentBadge status={selectedVideoEntry.isDerived ? 'modern-published' : 'legacy'} />
                <ContentBadge label={selectedVideoEntry.timingLabel} className={timingBadgeClass(selectedVideoEntry.timing)} />
                <ContentBadge label={selectedVideoEntry.contentTypeLabel} className={contentBadgeClass(selectedVideoEntry.contentType)} />
                <ContentBadge label={selectedVideoEntry.environmentLabel} className={environmentBadgeClass(selectedVideoEntry.environment)} />
              </span>
              {#if selectedVideoVariants.length > 1}
                <span class="segmented-control compact video-variant-control" aria-label="Video variant">
                  {#each selectedVideoVariants as variant}
                    <button
                      type="button"
                      class:active={selectedVideoVariant === variant.id}
                      on:click={() => {
                        selectedVideoVariant = variant.id;
                        currentVideoMs = 0;
                      }}
                    >
                      {variant.label}
                    </button>
                  {/each}
                </span>
              {/if}
              {#if selectedVideoEntry.countMarkers.length}
                <button
                  type="button"
                  class="pill video-count-toggle"
                  aria-pressed={showCountOverlay}
                  on:click={() => (showCountOverlay = !showCountOverlay)}
                >
                  {showCountOverlay ? 'Hide counts' : 'Show counts'}
                </button>
              {/if}
              {#if selectedClipEditUrl}
                <a class="pill video-clip-edit-link" href={selectedClipEditUrl}>Go to clip</a>
              {/if}
              {#if isEditingMoveDetails && selectedVideoEntry?.clipId}
                <button
                  type="button"
                  class="key-video-toggle"
                  aria-pressed={selectedVideoEntry.isKeyVideo}
                  title={selectedVideoEntry.isKeyVideo ? 'Remove key video' : 'Make key video'}
                  disabled={isSavingKeyVideo}
                  on:click={toggleSelectedVideoKeyState}
                >
                  {selectedVideoEntry.isKeyVideo ? '★' : '☆'}
                </button>
              {/if}
            </div>
          {/if}
        {:else}
          <div class="media-empty">
            <p class="media-empty-title">No main video</p>

            {#if move.videoLinks.length}
              <div class="stack media-legacy">
                <p class="muted media-empty-copy">Backup links:</p>
                <div class="move-linkrow">
                  {#each move.videoLinks as link}
                    <a class="pill move-linkpill" href={link} target="_blank" rel="noreferrer">Legacy link</a>
                  {/each}
                </div>
              </div>
            {/if}
          </div>
        {/if}
      </div>
    </div>

    <div class="meta-grid">
      {#if !relationshipWide}
        <div class="panel meta-card relationship-card">
          <div class="panel-header">
            <div class="panel-heading-row">
              <h3>Relationship to other moves</h3>
              {#if relationshipCard}
                <span class="muted">
                  {data.relationshipDiagram.meta.nodeCount} related
                </span>
              {/if}
            </div>
          </div>

          {#if relationshipCard}
            <MoveRelationshipDiagram diagram={data.relationshipDiagram} />
          {:else}
            <p class="muted relationship-empty">No relationship to other moves</p>
          {/if}
        </div>
      {/if}

      <div class="panel meta-card">
        <div class="panel-header">
          <div class="panel-heading-row">
            <h3>Move details</h3>
            {#if !isEditingMoveDetails}
              <button
                class="icon-button move-detail-edit-button"
                type="button"
                aria-label="Edit move details"
                title="Edit move details"
                on:click={startMoveDetailsEditor}
              >
                <svg viewBox="0 0 24 24" focusable="false" aria-hidden="true">
                  <path d="M21 7.2a5.6 5.6 0 0 1-7.1 6.8l-7.2 7.2a2.2 2.2 0 0 1-3.1-3.1l7.2-7.2A5.6 5.6 0 0 1 17.6 3l-3.1 3.1 3.4 3.4L21 7.2Z"></path>
                </svg>
              </button>
            {/if}
          </div>
        </div>
        {#if isEditingMoveDetails}
          <form class="move-detail-editor" on:submit|preventDefault={saveMoveDetails}>
            <div class="move-editor-grid move-detail-editor-grid">
              <label class="wide">
                <span>Name</span>
                <input bind:value={name} />
              </label>
              <label>
                <span>Level</span>
                <select bind:value={level}>
                  {#each levelOptions as option}
                    <option value={option}>{option || 'Unspecified'}</option>
                  {/each}
                </select>
              </label>
              <label>
                <span>Topic</span>
                <SearchablePicker
                  options={topicOptions}
                  selectedIds={selectedTopicIds}
                  query={topicQuery}
                  placeholder="Search topics"
                  addPlaceholder=""
                  ariaLabel="Search topics"
                  showSelected={true}
                  selectedPlacement="inside"
                  allowCreate={true}
                  createLabel="Use topic"
                  on:query={(event) => {
                    topicQuery = event.detail.query;
                  }}
                  on:select={(event) => {
                    topic = event.detail.option.label;
                    topicQuery = '';
                  }}
                  on:create={(event) => {
                    topic = event.detail.value;
                    topicQuery = '';
                    void persistMetadata('topic', event.detail.value);
                  }}
                  on:remove={() => {
                    topic = '';
                    topicQuery = '';
                  }}
                />
              </label>
              <label>
                <span>Type</span>
                <MoveTypeControl bind:value={moveType} />
              </label>
              <label>
                <span>Family</span>
                <SearchablePicker
                  options={familyOptions}
                  selectedIds={selectedFamilyIds}
                  query={familyQuery}
                  placeholder="Search families"
                  addPlaceholder=""
                  ariaLabel="Search families"
                  showSelected={true}
                  selectedPlacement="inside"
                  allowCreate={true}
                  createLabel="Use family"
                  on:query={(event) => {
                    familyQuery = event.detail.query;
                  }}
                  on:select={(event) => {
                    group = event.detail.option.label;
                    familyQuery = '';
                  }}
                  on:create={(event) => {
                    group = event.detail.value;
                    familyQuery = '';
                    void persistMetadata('family', event.detail.value);
                  }}
                  on:remove={() => {
                    group = '';
                    familyQuery = '';
                  }}
                />
              </label>
              <label>
                <span>Positions</span>
                <SearchablePicker
                  options={positionOptions}
                  selectedIds={selectedPositionIds}
                  query={positionsQuery}
                  placeholder=""
                  addPlaceholder=""
                  ariaLabel="Positions"
                  allowCreate={true}
                  createLabel="Use position"
                  on:query={(event) => (positionsQuery = event.detail.query)}
                  on:select={(event) => {
                    positions = event.detail.option.label;
                    positionsQuery = '';
                  }}
                  on:create={(event) => {
                    positions = event.detail.value;
                    positionsQuery = '';
                  }}
                  on:remove={() => {
                    positions = '';
                    positionsQuery = '';
                  }}
                />
              </label>
              <label class="wide">
                <span>Tags</span>
                <SearchablePicker
                  options={tagOptions}
                  selectedIds={selectedTagIds}
                  query={tagsQuery}
                  placeholder="Add tags"
                  addPlaceholder="Add tags"
                  ariaLabel="Tags"
                  selectedPlacement="inside"
                  allowCreate={true}
                  createLabel="Add tag"
                  on:query={(event) => (tagsQuery = event.detail.query)}
                  on:select={(event) => addTagValue(event.detail.option.label)}
                  on:create={(event) => addTagValue(event.detail.value)}
                  on:remove={(event) => removeTagValue(event.detail.id)}
                />
              </label>
              <label class="wide">
                <span>Authorship</span>
                <SearchablePicker
                  options={sourceOptions}
                  selectedIds={selectedSourceIds}
                  query={sourceQuery}
                  placeholder=""
                  addPlaceholder=""
                  ariaLabel="Authorship"
                  allowCreate={true}
                  createLabel="Use authorship"
                  on:query={(event) => (sourceQuery = event.detail.query)}
                  on:select={(event) => {
                    source = event.detail.option.label;
                    sourceQuery = '';
                  }}
                  on:create={(event) => {
                    source = event.detail.value;
                    sourceQuery = '';
                  }}
                  on:remove={() => {
                    source = '';
                    sourceQuery = '';
                  }}
                />
              </label>
              <label class="wide">
                <span>Description</span>
                <AutoResizeTextarea bind:value={description} />
              </label>
              <label class="wide">
                <span>Comments</span>
                <AutoResizeTextarea bind:value={comments} />
              </label>
            </div>
            <div class="move-detail-editor-actions">
              {#if detailEditorStatus}
                <span class="muted">{detailEditorStatus}</span>
              {/if}
              <button class="pill" type="button" disabled={isSavingMoveDetails} on:click={cancelMoveDetailsEditor}>Cancel</button>
              <button class="header-button" type="submit" disabled={isSavingMoveDetails}>Save</button>
            </div>
          </form>
        {:else}
          {#if movePropertyBadges.length}
            <div class="media-property-badges move-property-badges">
              {#each movePropertyBadges as badge}
                <ContentBadge label={badge} className="media-property-badge move-property-badge" />
              {/each}
            </div>
          {/if}
          <dl>
            {#each visibleMeta as [label, value]}
              <dt>{label}</dt>
              <dd>{value}</dd>
            {/each}
          </dl>
        {/if}
      </div>
    </div>
  </section>

  {#if relationshipWide}
    <section class="panel meta-card relationship-card relationship-card-wide">
      <div class="panel-header">
        <div class="panel-heading-row">
          <h3>Relationship to other moves</h3>
          {#if relationshipCard}
            <span class="muted">
              {data.relationshipDiagram.meta.nodeCount} related
            </span>
          {/if}
        </div>
      </div>

      {#if relationshipCard}
        <MoveRelationshipDiagram diagram={data.relationshipDiagram} />
      {:else}
        <p class="muted relationship-empty">No relationship to other moves</p>
      {/if}
    </section>
  {/if}

  {#if move.description && !isEditingMoveDetails}
    <section class="panel meta-card description-card">
      <div class="panel-header">
        <h3>Description</h3>
      </div>
      <div class="description-body">
        <p>{move.description}</p>
      </div>
    </section>
  {/if}
</div>
