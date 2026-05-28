<script lang="ts">
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import { publicationStatusFor } from '$lib/content-status';
  import type { ContentStatusTone } from '$lib/content-status';
  import type { DerivedClip, VideoContentType, VideoEnvironment, VideoOriginType, VideoTiming } from '$lib/types';

  type MediaAsset = {
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

  export let data: {
    timingOptions: Array<{ value: VideoTiming; label: string }>;
    contentTypeOptions: Array<{ value: VideoContentType; label: string }>;
    environmentOptions: Array<{ value: VideoEnvironment; label: string }>;
    assets: MediaAsset[];
    total: number;
    nextCursor: string | null;
    dancerOptions: string[];
    suggestions: {
      classWorkshops: string[];
      tags: string[];
    };
  };

  let assets = data.assets;
  let totalAssets = data.total;
  let nextCursor = data.nextCursor;
  let suggestions = data.suggestions;
  let isLoadingMore = false;
  let isDragging = false;
  let uploadStatus = '';
  let uploadFile: File | null = null;
  let mediaListElement: HTMLDivElement | null = null;
  let publicationFilter: 'all' | 'published' | 'unpublished' | 'draft' = 'all';
  let environmentFilter: 'all' | VideoEnvironment = 'all';
  let selectedDancerIds: string[] = [];
  let dancerQuery = '';
  let loadedFilterKey = 'all|all|';
  let filterRequestId = 0;

  $: dancerOptions = data.dancerOptions.map((dancer) => ({ id: dancer, label: dancer }));
  $: desiredFilterKey = mediaFilterKey();
  $: if (desiredFilterKey !== loadedFilterKey) {
    void loadFilteredMedia(desiredFilterKey);
  }

  $: filteredAssets = assets.filter((asset) => {
    if (!matchesPublicationFilter(asset)) return false;
    if (environmentFilter !== 'all' && asset.environment !== environmentFilter) return false;
    if (selectedDancerIds.length && !selectedDancerIds.every((dancer) => asset.dancers.includes(dancer))) return false;
    return true;
  });

  $: mediaGroups = filteredAssets.reduce(
    (groups, asset) => {
      const month = monthLabel(asset.createdAt);
      let group = groups.find((entry) => entry.month === month);
      if (!group) {
        group = { month, assets: [] as MediaAsset[] };
        groups.push(group);
      }
      group.assets.push(asset);
      return groups;
    },
    [] as Array<{ month: string; assets: MediaAsset[] }>
  );

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }

  function editUrl(asset: MediaAsset) {
    return `/media/edit/${encodeURIComponent(asset.id)}`;
  }

  function formatDate(value: string | null) {
    if (!value) return 'Not recorded';
    const date = new Date(value);
    if (Number.isNaN(date.getTime())) return value;
    return new Intl.DateTimeFormat('en-AU', { day: '2-digit', month: 'short', year: 'numeric' }).format(date);
  }

  function monthLabel(value: string) {
    const date = new Date(value);
    if (Number.isNaN(date.getTime())) return 'Unknown upload month';
    return new Intl.DateTimeFormat('en-AU', { month: 'long', year: 'numeric' }).format(date);
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

  function assetPublicationStatus(asset: MediaAsset) {
    if (!asset.clips.length) return 'unpublished';
    if (asset.clips.some((clip) => clipPublicationStatus(clip) === 'changed-unpublished')) return 'draft';
    if (asset.clips.some((clip) => clipPublicationStatus(clip) === 'modern-published')) return 'published';
    return 'unpublished';
  }

  function assetPublicationTone(asset: MediaAsset): ContentStatusTone {
    const status = assetPublicationStatus(asset);
    if (status === 'draft') return 'warning';
    if (status === 'published') return 'success';
    return 'neutral';
  }

  function matchesPublicationFilter(asset: MediaAsset) {
    if (publicationFilter === 'all') return true;
    return assetPublicationStatus(asset) === publicationFilter;
  }

  function mediaFilterKey() {
    return [publicationFilter, environmentFilter, [...selectedDancerIds].sort().join(',')].join('|');
  }

  function mediaLibraryUrl(cursor: string | null = null) {
    const params = new URLSearchParams();
    params.set('limit', '50');
    params.set('publication', publicationFilter);
    params.set('environment', environmentFilter);
    if (cursor) {
      params.set('cursor', cursor);
    }
    selectedDancerIds.forEach((dancer) => params.append('dancer', dancer));
    return `/api/media/library?${params.toString()}`;
  }

  async function loadFilteredMedia(filterKey: string) {
    const requestId = ++filterRequestId;
    isLoadingMore = true;
    const response = await fetch(mediaLibraryUrl());

    if (requestId !== filterRequestId) {
      return;
    }

    if (response.ok) {
      const payload = await response.json();
      assets = payload.assets;
      totalAssets = payload.total;
      nextCursor = payload.nextCursor;
      suggestions = payload.suggestions;
      loadedFilterKey = filterKey;
    }

    isLoadingMore = false;
  }

  function defaultDisplayNameFor(file: File) {
    return file.name.replace(/\.[^.]+$/, '').replace(/[_-]+/g, ' ').trim() || file.name;
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

    window.location.href = editUrl(payload.asset);
  }

  function handleDrop(event: DragEvent) {
    event.preventDefault();
    isDragging = false;
    const file = event.dataTransfer?.files?.[0] ?? null;
    if (file) {
      void uploadSourceFile(file);
    }
  }

  async function loadMoreMedia() {
    if (!nextCursor || isLoadingMore) return;

    isLoadingMore = true;
    const response = await fetch(mediaLibraryUrl(nextCursor));

    if (response.ok) {
      const payload = await response.json();
      const existingIds = new Set(assets.map((asset) => asset.id));
      assets = [...assets, ...payload.assets.filter((asset: MediaAsset) => !existingIds.has(asset.id))];
      totalAssets = payload.total;
      nextCursor = payload.nextCursor;
      suggestions = payload.suggestions;
    }

    isLoadingMore = false;
  }

  function handleMediaScroll() {
    if (!mediaListElement) return;
    const remaining = mediaListElement.scrollHeight - mediaListElement.scrollTop - mediaListElement.clientHeight;
    if (remaining < 500) {
      void loadMoreMedia();
    }
  }
</script>

<svelte:head>
  <title>Media | Salsa Encyclopedia</title>
</svelte:head>

<div class="media-gallery-page">
  <div class="media-gallery-scroll" bind:this={mediaListElement} on:scroll={handleMediaScroll}>
    <section class="media-filter-bar" aria-label="Media filters">
      <div class="media-filter-controls">
        <div class="media-filter-group">
          <span class="media-filter-label">Status</span>
          <div class="segmented-control compact media-filter-segment" role="radiogroup" aria-label="Publication status">
            <button type="button" class:active={publicationFilter === 'all'} on:click={() => (publicationFilter = 'all')}>All</button>
            <button type="button" class:active={publicationFilter === 'published'} on:click={() => (publicationFilter = 'published')}>Published</button>
            <button type="button" class:active={publicationFilter === 'draft'} on:click={() => (publicationFilter = 'draft')}>Draft</button>
            <button type="button" class:active={publicationFilter === 'unpublished'} on:click={() => (publicationFilter = 'unpublished')}>Unpublished</button>
          </div>
          <select class="media-filter-select media-filter-status-select" bind:value={publicationFilter} aria-label="Publication status">
            <option value="all">All</option>
            <option value="published">Published</option>
            <option value="draft">Draft</option>
            <option value="unpublished">Unpublished</option>
          </select>
        </div>
        <div class="media-filter-group">
          <span class="media-filter-label">Type</span>
          <div class="segmented-control compact media-filter-segment" role="radiogroup" aria-label="Environment">
            <button type="button" class:active={environmentFilter === 'all'} on:click={() => (environmentFilter = 'all')}>All</button>
            <button type="button" class:active={environmentFilter === 'class'} on:click={() => (environmentFilter = 'class')}>Classes</button>
            <button type="button" class:active={environmentFilter === 'social'} on:click={() => (environmentFilter = 'social')}>Socials</button>
          </div>
          <select class="media-filter-select media-filter-type-select" bind:value={environmentFilter} aria-label="Environment">
            <option value="all">All</option>
            <option value="class">Classes</option>
            <option value="social">Socials</option>
          </select>
        </div>
      </div>
      <div class="media-filter-dancer">
        <span class="media-filter-label">Dancer</span>
        <SearchablePicker
          options={dancerOptions}
          selectedIds={selectedDancerIds}
          query={dancerQuery}
          placeholder="Any dancer"
          addPlaceholder="Add dancer"
          ariaLabel="Filter by dancer"
          selectedPlacement="inside"
          on:query={(event) => (dancerQuery = event.detail.query)}
          on:select={(event) => (selectedDancerIds = [...selectedDancerIds, event.detail.id])}
          on:remove={(event) => (selectedDancerIds = selectedDancerIds.filter((id) => id !== event.detail.id))}
        />
      </div>
      <span class="muted media-filter-count">{filteredAssets.length} / {totalAssets}</span>
    </section>

    <div class="media-card-grid media-source-grid media-upload-grid">
      <section
        class={`media-upload-tile compact ${isDragging ? 'drag-over' : ''}`}
        on:dragover={(event) => {
          event.preventDefault();
          isDragging = true;
        }}
        on:dragleave={() => (isDragging = false)}
        on:drop={handleDrop}
      >
        <div class="media-upload-form">
          <label class="media-file-target media-file-target-large">
            <span>{uploadFile ? `Uploading ${uploadFile.name}` : 'Add new source video'}</span>
            <input
              type="file"
              accept="video/*"
              on:change={(event) => void uploadSourceFile((event.currentTarget as HTMLInputElement).files?.[0] ?? null)}
            />
          </label>

          {#if uploadStatus}
            <span class="muted">{uploadStatus}</span>
          {/if}
        </div>
      </section>
    </div>

    {#each mediaGroups as group}
      <section class="media-month-section">
        <h3>{group.month}</h3>
        <div class="media-card-grid media-source-grid">
          {#each group.assets as asset}
            <a class="media-gallery-card" href={editUrl(asset)}>
              <span class="media-gallery-poster">
                {#if asset.posterFile}
                  <img src={posterUrl(asset.posterFile)} alt="" loading="lazy" />
                {:else}
                  <span>No preview</span>
                {/if}
              </span>
              <span class="media-gallery-card-body">
                <strong>{asset.displayName}</strong>
                <span class="content-badge-list">
                  <ContentBadge label={assetPublicationStatus(asset)} tone={assetPublicationTone(asset)} />
                  <ContentBadge label={timingLabel(asset.timing)} className={timingBadgeClass(asset.timing)} />
                  <ContentBadge label={contentTypeLabel(asset.contentType)} className={contentBadgeClass(asset.contentType)} />
                  <ContentBadge label={environmentLabel(asset.environment)} className={environmentBadgeClass(asset.environment)} />
                </span>
                <span>Uploaded {formatDate(asset.createdAt)} · {asset.clips.length} clips</span>
                {#if asset.recordDate}<span>Recorded {formatDate(asset.recordDate)}</span>{/if}
                {#if asset.classWorkshop}<span>{asset.classWorkshop}</span>{/if}
                {#if asset.tags.length}<span>{asset.tags.join(', ')}</span>{/if}
              </span>
            </a>
          {/each}
        </div>
      </section>
    {/each}

    {#if nextCursor}
      <button class="media-load-more" type="button" disabled={isLoadingMore} on:click={() => void loadMoreMedia()}>
        {isLoadingMore ? 'Loading...' : 'Load more'}
      </button>
    {/if}
  </div>

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
