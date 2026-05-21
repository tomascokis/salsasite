<script lang="ts">
  import { browser } from '$app/environment';
  import { onDestroy } from 'svelte';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import MoveRelationshipDiagram from '$lib/components/MoveRelationshipDiagram.svelte';
  import type {
    MoveVideoEntry,
    MoveRecord,
    RawMoveReferenceRecord,
    RelationshipDiagram
  } from '$lib/types';

  export let data: {
    move: MoveRecord;
    rawReference: RawMoveReferenceRecord | null;
    relationshipDiagram: RelationshipDiagram;
    videos: MoveVideoEntry[];
  };

  const VIDEO_VOLUME_SESSION_KEY = 'salsa-encyclopedia:video-volume';
  let videos = data.videos;
  let selectedVideo = 0;
  let videoElement: HTMLVideoElement | null = null;
  let posterPollTimeout: ReturnType<typeof setTimeout> | null = null;
  let posterPollTarget: string | null = null;
  let savedSessionVolume: number | null = null;
  let selectedVideoEntry: MoveVideoEntry | null = null;
  let showCountOverlay = false;
  let currentVideoMs = 0;
  const unavailablePosterVideos = new Set<string>();
  const family = data.rawReference?.family?.trim() || null;

  function normalizeSubtitleValue(value: string) {
    return value
      .toLocaleLowerCase()
      .replace(/[^\p{L}\p{N}]+/gu, ' ')
      .trim()
      .split(/\s+/)
      .map((token) => (token.length > 3 && token.endsWith('s') ? token.slice(0, -1) : token))
      .join(' ');
  }

  const subtitleParts = [data.move.topic?.trim() || null, family].filter((value, index, values) => {
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
  const subtitle = subtitleParts.join(' • ');

  const visibleMeta = [
    ['ID', data.move.id],
    ['Level', data.move.level],
    ['Type', data.move.type],
    ['Group', data.move.group],
    ['Positions', data.move.positions],
    ['See also', data.move.seeAlso],
    ['Tags', data.move.tags],
    ['Source', data.move.source]
  ].filter(([, value]) => value);

  const movePropertyBadges = [
    data.move.level ? `Level ${data.move.level}` : null,
    data.move.type,
    data.move.group,
    data.move.positions
  ].filter((value): value is string => Boolean(value));

  const relationshipCard = data.relationshipDiagram.meta.hasDiagram;
  const relationshipWide = data.relationshipDiagram.meta.isLarge;

  function mediaUrl(file: string) {
    return `/media/${encodeURIComponent(file)}`;
  }

  function timingBadgeClass(entry: MoveVideoEntry) {
    return `badge-timing-${entry.timing}`;
  }

  function contentBadgeClass(entry: MoveVideoEntry) {
    return `badge-content-${entry.contentType}`;
  }

  function environmentBadgeClass(entry: MoveVideoEntry) {
    return `badge-environment-${entry.environment}`;
  }

  function videoContentStatus(entry: MoveVideoEntry): 'modern-published' | 'legacy' {
    return entry.isDerived ? 'modern-published' : 'legacy';
  }

  function clipEditUrl(entry: MoveVideoEntry) {
    if (!entry.sourceAssetId || !entry.clipId) {
      return null;
    }

    return `/media/edit/${encodeURIComponent(entry.sourceAssetId)}?clip=${encodeURIComponent(entry.clipId)}`;
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
    return videos[selectedVideo]?.filePath ?? null;
  }

  function normalizeVolume(value: number) {
    if (Number.isNaN(value)) {
      return 1;
    }

    return Math.min(1, Math.max(0, value));
  }

  function getSavedSessionVolume() {
    if (!browser) {
      return 1;
    }

    if (savedSessionVolume !== null) {
      return savedSessionVolume;
    }

    const rawValue = window.sessionStorage.getItem(VIDEO_VOLUME_SESSION_KEY);
    const parsedValue = rawValue === null ? 1 : Number(rawValue);
    savedSessionVolume = normalizeVolume(parsedValue);
    return savedSessionVolume;
  }

  function applySavedSessionVolume() {
    if (!browser || !videoElement) {
      return;
    }

    videoElement.volume = getSavedSessionVolume();
  }

  function handleVolumeChange() {
    if (!browser || !videoElement) {
      return;
    }

    savedSessionVolume = normalizeVolume(videoElement.volume);
    window.sessionStorage.setItem(VIDEO_VOLUME_SESSION_KEY, String(savedSessionVolume));
  }

  function handleVideoTimeUpdate() {
    currentVideoMs = Math.round((videoElement?.currentTime ?? 0) * 1000);
  }

  function visibleCountMarker(entry: MoveVideoEntry | null, currentMs: number, enabled: boolean) {
    if (!entry || !enabled) {
      return null;
    }

    let visible: MoveVideoEntry['countMarkers'][number] | null = null;
    for (const marker of [...entry.countMarkers].sort((left, right) => left.ms - right.ms)) {
      if (marker.ms > currentMs) {
        break;
      }
      visible = marker.clear ? null : marker;
    }
    return visible;
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
    const shouldPoll = Boolean(videoFile && !posterFile && !unavailablePosterVideos.has(videoFile));

    if (!shouldPoll) {
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
    applySavedSessionVolume();
  }

  $: selectedVideoEntry = videos[selectedVideo] ?? null;
  $: selectedClipEditUrl = selectedVideoEntry ? clipEditUrl(selectedVideoEntry) : null;
  $: selectedCountMarker = visibleCountMarker(selectedVideoEntry, currentVideoMs, showCountOverlay);

  $: ensurePosterPolling();

  onDestroy(() => {
    stopPosterPolling();
  });
</script>

<svelte:head>
  <title>{data.move.name} | Salsa Encyclopedia</title>
</svelte:head>

<div class="move-layout">
  <a class="pill move-backlink" href="/">Back to overview</a>

  <section class="move-header">
    <div class="panel video-panel">
      <div class="panel-header move-titleblock">
        <div>
          <h2>{data.move.name}</h2>
          {#if subtitle}
            <p class="muted move-subtitle">{subtitle}</p>
          {/if}
        </div>
        <a class="header-button" href={`/moves/${data.move.slug}/edit`}>Edit</a>
      </div>
      <div class="media-stage">
        {#if videos.length}
          {#if videos.length > 1}
            <div class="tab-list">
              {#each videos as video, index}
                <button
                  type="button"
                  class:active={selectedVideo === index}
                  on:click={() => {
                    selectedVideo = index;
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
                  <span class="video-tab-label">Video {index + 1}</span>
                </button>
              {/each}
            </div>
          {/if}
          {#key `${videos[selectedVideo]?.filePath ?? ''}::${currentPosterUrl() ?? ''}`}
            <div class="video-frame">
              <video
                bind:this={videoElement}
                controls
                on:volumechange={handleVolumeChange}
                on:timeupdate={handleVideoTimeUpdate}
                preload="metadata"
                poster={currentPosterUrl()}
                src={mediaUrl(videos[selectedVideo].filePath)}
              ></video>
              {#if selectedVideoEntry && selectedCountMarker}
                <span class={countOverlayClass(selectedVideoEntry)}>
                  {selectedCountMarker.count}
                </span>
              {/if}
            </div>
          {/key}

          {#if selectedVideoEntry}
            <div class="video-provenance">
              <div class="video-provenance-row">
                <strong>Timing</strong>
                <ContentBadge label={selectedVideoEntry.timingLabel} className={timingBadgeClass(selectedVideoEntry)} />
              </div>
              <div class="video-provenance-row">
                <strong>Type</strong>
                <ContentBadge label={selectedVideoEntry.contentTypeLabel} className={contentBadgeClass(selectedVideoEntry)} />
              </div>
              <div class="video-provenance-row">
                <strong>Environment</strong>
                <ContentBadge label={selectedVideoEntry.environmentLabel} className={environmentBadgeClass(selectedVideoEntry)} />
              </div>
              <div class="video-provenance-row">
                <strong>Dancers</strong>
                <span>{selectedVideoEntry.sourceDancers.length ? selectedVideoEntry.sourceDancers.join(', ') : selectedVideoEntry.dancers.length ? selectedVideoEntry.dancers.join(', ') : 'Not recorded'}</span>
              </div>
              <div class="video-provenance-row">
                <strong>Origin</strong>
                <span class="video-origin-summary">
                  <ContentBadge status={videoContentStatus(selectedVideoEntry)} />
                  {#if selectedVideoEntry.isDerived}
                    <span>Derived from {selectedVideoEntry.sourceDisplayName ?? 'source video'}</span>
                    {#if selectedClipEditUrl}
                      <a class="pill video-clip-edit-link" href={selectedClipEditUrl}>Go to clip</a>
                    {/if}
                  {:else if selectedVideoEntry.kind === 'source'}
                    <span>Full source video</span>
                  {:else}
                    <span>Standalone move video</span>
                  {/if}
                </span>
              </div>
              {#if selectedVideoEntry.countMarkers.length}
                <div class="video-provenance-row">
                  <strong>Counts</strong>
                  <button
                    type="button"
                    class="pill video-count-toggle"
                    aria-pressed={showCountOverlay}
                    on:click={() => (showCountOverlay = !showCountOverlay)}
                  >
                    {showCountOverlay ? 'Hide overlay' : 'Show overlay'}
                  </button>
                </div>
              {/if}
              {#if selectedVideoEntry.sourceRecordDate || selectedVideoEntry.recordDate}
                <div class="video-provenance-row">
                  <strong>Recorded</strong>
                  <span>{selectedVideoEntry.sourceRecordDate ?? selectedVideoEntry.recordDate}</span>
                </div>
              {/if}
              {#if selectedVideoEntry.sourceClassWorkshop || selectedVideoEntry.classWorkshop}
                <div class="video-provenance-row">
                  <strong>Class</strong>
                  <span>{selectedVideoEntry.sourceClassWorkshop ?? selectedVideoEntry.classWorkshop}</span>
                </div>
              {/if}
              {#if selectedVideoEntry.sourceTags.length || selectedVideoEntry.tags.length}
                <div class="video-provenance-row">
                  <strong>Tags</strong>
                  <span>{selectedVideoEntry.sourceTags.length ? selectedVideoEntry.sourceTags.join(', ') : selectedVideoEntry.tags.join(', ')}</span>
                </div>
              {/if}
              {#if selectedVideoEntry.sourceNotes || selectedVideoEntry.notes}
                <div class="video-provenance-row notes">
                  <strong>Notes</strong>
                  <span>{selectedVideoEntry.sourceNotes ?? selectedVideoEntry.notes}</span>
                </div>
              {/if}
            </div>
          {/if}
        {:else}
          <div class="media-empty">
            <p class="media-empty-title">No main video</p>

            {#if data.move.videoLinks.length}
              <div class="stack media-legacy">
                <p class="muted media-empty-copy">Backup links:</p>
                <div class="move-linkrow">
                  {#each data.move.videoLinks as link}
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
          <h3>Move details</h3>
        </div>
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

  {#if data.move.description}
    <section class="panel meta-card description-card">
      <div class="panel-header">
        <h3>Description</h3>
      </div>
      <div class="description-body">
        <p>{data.move.description}</p>
      </div>
    </section>
  {/if}
</div>
