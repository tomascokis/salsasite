<script lang="ts">
  import { browser } from '$app/environment';
  import { onDestroy } from 'svelte';
  import MoveHoverPreview from '$lib/components/MoveHoverPreview.svelte';
  import { colorForProgress } from '$lib/progress-ui';
  import type { ProgressView, ProgressViewEntryMove } from '$lib/types';

  export let data: {
    views: ProgressView[];
  };

  let activeIndex = 0;
  let hoverSelectionTimeout: ReturnType<typeof setTimeout> | null = null;
  let hoverPreviewTimeout: ReturnType<typeof setTimeout> | null = null;
  let selectedPreviewMoveId: string | null = null;
  let hoveredPreview: {
    id: string;
    name: string;
    videoFile: string | null;
    left: number;
    top: number;
  } | null = null;

  $: orderedColumns = data.views[activeIndex]?.columns
    .map((column) => ({
      ...column,
      entries: [...column.entries].sort((a, b) => (a.layoutOrder ?? 0) - (b.layoutOrder ?? 0))
    }))
    .sort((a, b) => a.column - b.column) ?? [];

  function isEmphasis(type: string | null) {
    return type === 'Addition' || type === 'Variation';
  }

  function typeClass(type: string | null) {
    if (type === 'Addition') return 'addition';
    if (type === 'Variation') return 'variation';
    return '';
  }

  function setActiveIndex(index: number) {
    clearHoveredPreview();
    activeIndex = index;
  }

  function updateHoveredPreviewFromRow(row: HTMLElement, entry: ProgressViewEntryMove) {
    if (!browser || !entry.id) {
      clearHoveredPreview();
      return;
    }

    const rowRect = row.getBoundingClientRect();
    const previewWidth = 240;
    const previewHeight = 190;
    const minLeft = 8;
    const maxLeft = window.innerWidth - previewWidth - 8;
    const minTop = 8;
    const maxTop = window.innerHeight - previewHeight - 8;
    const belowTop = rowRect.bottom + 8;
    const aboveTop = rowRect.top - previewHeight - 8;
    const unclampedTop = belowTop <= maxTop ? belowTop : aboveTop;

    hoveredPreview = {
      id: entry.id,
      name: entry.name || entry.id,
      videoFile: entry.previewVideoFile ?? null,
      left: Math.max(minLeft, Math.min(rowRect.left, Math.max(minLeft, maxLeft))),
      top: Math.max(minTop, Math.min(unclampedTop, Math.max(minTop, maxTop)))
    };
  }

  function updateHoveredPreview(event: FocusEvent, entry: ProgressViewEntryMove) {
    const target = event.currentTarget;
    if (!(target instanceof HTMLElement)) {
      clearHoveredPreview();
      return;
    }

    clearHoverPreviewTimeout();
    selectedPreviewMoveId = entry.id;
    updateHoveredPreviewFromRow(target, entry);
  }

  function scheduleHoveredPreview(event: MouseEvent | PointerEvent, entry: ProgressViewEntryMove) {
    const target = event.currentTarget;
    if (!(target instanceof HTMLElement)) {
      clearHoveredPreview();
      return;
    }

    scheduleHoverSelection(entry);
    clearHoverPreviewTimeout();
    hoverPreviewTimeout = setTimeout(() => updateHoveredPreviewFromRow(target, entry), 1000);
  }

  function scheduleHoverSelection(entry: ProgressViewEntryMove) {
    clearHoverSelectionTimeout();
    hoverSelectionTimeout = setTimeout(() => {
      selectedPreviewMoveId = entry.id;
    }, 100);
  }

  function clearHoverSelectionTimeout() {
    if (hoverSelectionTimeout) {
      clearTimeout(hoverSelectionTimeout);
      hoverSelectionTimeout = null;
    }
  }

  function clearHoverPreviewTimeout() {
    if (hoverPreviewTimeout) {
      clearTimeout(hoverPreviewTimeout);
      hoverPreviewTimeout = null;
    }
  }

  function clearHoveredPreview() {
    clearHoverSelectionTimeout();
    clearHoverPreviewTimeout();
    selectedPreviewMoveId = null;
    hoveredPreview = null;
  }

  onDestroy(clearHoveredPreview);
</script>

<div class="stack">
  <section class="panel">
    <div style="padding: 1rem 1.1rem" class="stack">
      <h2 class="sr-only">Progress</h2>
      <div class="progress-controls">
        <a class="header-button progress-log-button" href="/progress/editor">Log new progress</a>
        <div class="progress-control-divider" aria-hidden="true"></div>
        <div class="snapshot-tabs" aria-label="Progress snapshots">
          {#each data.views as view, index}
            <button class:active={activeIndex === index} on:click={() => setActiveIndex(index)}>
              {view.label}
            </button>
          {/each}
        </div>
      </div>

      {#if data.views[activeIndex]}
        <div class="summary-strip">
          <span><strong>Total:</strong> {data.views[activeIndex].summary.totalMoves}</span>
          <span><strong>Prepped:</strong> {data.views[activeIndex].summary.preppedCount}</span>
          <span><strong>Good Sequencing:</strong> {data.views[activeIndex].summary.goodSequencingCount}</span>
          <span><strong>Good Success:</strong> {data.views[activeIndex].summary.goodSuccessCount}</span>
        </div>

        <div class="dashboard-columns">
          {#each orderedColumns as column (column.column)}
            <div class="dashboard-column">
              {#each column.entries as entry (entry.layoutOrder ?? (entry.kind === 'move' ? entry.id : entry.name))}
                {#if entry.kind === 'title'}
                  <div class="progress-row title-row">
                    <div class="title-cell">{entry.name}</div>
                  </div>
                {:else}
                  <a
                    class={`progress-row data-row ${typeClass(entry.type)}`}
                    class:preview-selected={selectedPreviewMoveId === entry.id}
                    href={`/moves/${entry.slug}`}
                    on:pointerenter={(event) => scheduleHoveredPreview(event, entry)}
                    on:mouseenter={(event) => scheduleHoveredPreview(event, entry)}
                    on:pointerleave={clearHoveredPreview}
                    on:mouseleave={clearHoveredPreview}
                    on:focus={(event) => updateHoveredPreview(event, entry)}
                    on:blur={clearHoveredPreview}
                  >
                    <div class="level-cell">{entry.level ?? ''}</div>
                    <div class={`name-cell ${isEmphasis(entry.type) ? 'emphasis' : ''}`}>{entry.name}</div>
                    <div class="progress-dot" style={`background:${colorForProgress('prep', entry.prep)}`}></div>
                    <div class="progress-dot" style={`background:${colorForProgress('sequ', entry.sequ)}`}></div>
                    <div class="progress-dot" style={`background:${colorForProgress('succ', entry.succ)}`}></div>
                  </a>
                {/if}
              {/each}
            </div>
          {/each}
        </div>
        {#if hoveredPreview}
          <MoveHoverPreview
            id={hoveredPreview.id}
            name={hoveredPreview.name}
            videoFile={hoveredPreview.videoFile}
            left={hoveredPreview.left}
            top={hoveredPreview.top}
          />
        {/if}
      {/if}
    </div>
  </section>
</div>

<style>
  .data-row.preview-selected {
    position: relative;
    z-index: 1;
    background: #eef6fc;
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.85),
      inset 0 -1px 0 rgba(32, 75, 114, 0.3),
      0 1px 2px rgba(32, 75, 114, 0.22);
    animation: move-row-selected 180ms ease-out;
  }

  @keyframes move-row-selected {
    0% {
      box-shadow:
        inset 0 0 0 rgba(255, 255, 255, 0),
        inset 0 0 0 rgba(32, 75, 114, 0),
        0 0 0 rgba(32, 75, 114, 0);
      transform: translateY(0);
    }

    45% {
      box-shadow:
        inset 0 1px 0 rgba(255, 255, 255, 0.95),
        inset 0 -2px 0 rgba(32, 75, 114, 0.34),
        0 2px 4px rgba(32, 75, 114, 0.24);
      transform: translateY(-1px);
    }

    100% {
      box-shadow:
        inset 0 1px 0 rgba(255, 255, 255, 0.85),
        inset 0 -1px 0 rgba(32, 75, 114, 0.3),
        0 1px 2px rgba(32, 75, 114, 0.22);
      transform: translateY(0);
    }
  }
</style>
