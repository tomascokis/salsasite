<script lang="ts">
  import { browser } from '$app/environment';
  import { onDestroy, onMount, tick } from 'svelte';
  import MoveHoverPreview from '$lib/components/MoveHoverPreview.svelte';
  import type { LayoutColumn, LayoutEntry, SearchIndexEntry, SiteManifest } from '$lib/types';

  export let data: {
    manifest: SiteManifest;
    layout: LayoutColumn[];
    searchIndex: SearchIndexEntry[];
  };

  let query = '';
  let showVariations = true;
  let showAdditions = true;
  let columnMetrics: Record<number, { maxHeight: number; overflowing: boolean; contentHeight: number; availableHeight: number; cutoffIndex?: number }> = {};
  let expandedColumns: Record<number, boolean> = {};
  let measureQueued = false;
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

  $: orderedLayout = data.layout
    .map((column) => ({
      ...column,
      entries: [...column.entries].sort((a, b) => (a.layoutOrder ?? 0) - (b.layoutOrder ?? 0))
    }))
    .sort((a, b) => a.column - b.column);
  $: normalizedQuery = normalizeComparable(query);
  $: matchingMoveIds = normalizedQuery
    ? new Set(
        data.searchIndex
          .filter((entry) =>
            normalizeComparable(`${entry.title ?? ''} ${entry.topic ?? ''} ${entry.text}`).includes(normalizedQuery)
          )
          .map((entry) => normalizeComparable(entry.id))
      )
    : null;
  $: visibleLayout = orderedLayout
    .map((column) => ({
      ...column,
      entries: preserveRelevantTitles(
        column.entries.filter((entry) => {
          if (entry.entryType !== 'Data') return true;
          if (entry.type === 'Variation' && !showVariations) return false;
          if (entry.type === 'Addition' && !showAdditions) return false;
          if (matchingMoveIds && !matchingMoveIds.has(normalizeComparable(entry.id))) return false;
          return true;
        })
      )
    }))
    .filter((column) => column.entries.some((entry) => entry.entryType === 'Data'));
  $: visibleLayoutWithOverflow = visibleLayout.flatMap((column) => {
    const result: any[] = [column];
    if (expandedColumns[column.column] && columnMetrics[column.column]?.cutoffIndex !== undefined) {
      const cutoffIndex = columnMetrics[column.column]!.cutoffIndex!;
      const overflowEntries = column.entries.slice(cutoffIndex);
      result.push({
        ...column,
        entries: overflowEntries,
        isOverflowColumn: true,
        overflowSourceColumn: column.column
      });
    }
    return result;
  });

  function isEmphasis(type: string | null) {
    return type === 'Addition' || type === 'Variation';
  }

  function typeClass(type: string | null) {
    if (type === 'Addition') return 'addition';
    if (type === 'Variation') return 'variation';
    return '';
  }

  function toggleColumn(column: number) {
    clearHoveredPreview();
    expandedColumns = { ...expandedColumns, [column]: !expandedColumns[column] };
  }

  function updateHoveredPreviewFromRow(row: HTMLElement, entry: LayoutEntry) {
    if (!browser || entry.entryType !== 'Data' || !entry.id) {
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
      name: entry.name ?? entry.id,
      videoFile: entry.previewVideoFile ?? null,
      left: Math.max(minLeft, Math.min(rowRect.left, Math.max(minLeft, maxLeft))),
      top: Math.max(minTop, Math.min(unclampedTop, Math.max(minTop, maxTop)))
    };
  }

  function updateHoveredPreview(event: FocusEvent, entry: LayoutEntry) {
    const target = event.currentTarget;
    if (!(target instanceof HTMLElement)) {
      clearHoveredPreview();
      return;
    }

    clearHoverPreviewTimeout();
    selectedPreviewMoveId = entry.id;
    updateHoveredPreviewFromRow(target, entry);
  }

  function scheduleHoveredPreview(event: MouseEvent | PointerEvent, entry: LayoutEntry) {
    const target = event.currentTarget;
    if (!(target instanceof HTMLElement)) {
      clearHoveredPreview();
      return;
    }

    scheduleHoverSelection(entry);
    clearHoverPreviewTimeout();
    hoverPreviewTimeout = setTimeout(() => updateHoveredPreviewFromRow(target, entry), 1000);
  }

  function scheduleHoverSelection(entry: LayoutEntry) {
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

  async function measureColumns() {
    await tick();

    const nodes = Array.from(document.querySelectorAll<HTMLElement>('.dashboard-column-content'));
    const nextMetrics: Record<number, { maxHeight: number; overflowing: boolean; contentHeight: number; availableHeight: number; cutoffIndex?: number }> = {};
    const nextExpandedColumns = { ...expandedColumns };

    for (const node of nodes) {
      const column = Number(node.dataset.column);
      if (Number.isNaN(column)) continue;

      const rect = node.getBoundingClientRect();
      const viewportHeight = document.documentElement.clientHeight;
      const buttonReserve = 56;
      const spaceToBottom = Math.max(0, Math.floor(viewportHeight - rect.top - buttonReserve));
      const collapsedHeight = Math.max(spaceToBottom, 0);
      const contentHeight = node.scrollHeight;
      const overflowing = contentHeight > collapsedHeight + 1;

      // Find the cutoff index - which entry is the last one visible
      let cutoffIndex = undefined;
      if (overflowing) {
        const rows = Array.from(node.querySelectorAll<HTMLElement>('.dashboard-row'));
        let cumulativeHeight = 0;
        
        for (let i = 0; i < rows.length; i++) {
          const row = rows[i];
          const rowHeight = row.getBoundingClientRect().height;
          if (cumulativeHeight + rowHeight > collapsedHeight) {
            cutoffIndex = i;
            break;
          }
          cumulativeHeight += rowHeight;
        }
      }

      nextMetrics[column] = {
        maxHeight: collapsedHeight,
        overflowing,
        contentHeight,
        availableHeight: spaceToBottom,
        cutoffIndex
      };

      if (!overflowing) {
        nextExpandedColumns[column] = false;
      }
    }

    columnMetrics = { ...nextMetrics };
    expandedColumns = { ...nextExpandedColumns };
  }

  function scheduleColumnMeasurement() {
    if (!browser || measureQueued) return;
    measureQueued = true;
    requestAnimationFrame(async () => {
      measureQueued = false;
      await measureColumns();
    });
  }

  onMount(() => {
    scheduleColumnMeasurement();

    const handleResize = () => scheduleColumnMeasurement();
    window.addEventListener('resize', handleResize);

    return () => window.removeEventListener('resize', handleResize);
  });

  onDestroy(clearHoveredPreview);

  $: if (browser) {
    visibleLayout;
    normalizedQuery;
    scheduleColumnMeasurement();
  }

  function preserveRelevantTitles(entries: LayoutEntry[]) {
    const result: LayoutEntry[] = [];
    let pendingTitles: LayoutEntry[] = [];

    for (const entry of entries) {
      if (entry.entryType !== 'Data') {
        pendingTitles = [...pendingTitles, entry];
        continue;
      }

      result.push(...pendingTitles, entry);
      pendingTitles = [];
    }

    return result;
  }

  function normalizeComparable(value: string | null | undefined) {
    return String(value ?? '')
      .toLocaleLowerCase()
      .replace(/\s+/g, ' ')
      .trim();
  }
</script>

<div class="stack">
  <section class="dashboard-page">
    <div class="dashboard-titlebar">
      <h1 class="sr-only">Overview</h1>
      <div class="dashboard-tools">
        <div class="dashboard-filters">
          <span class="filter-label">Show:</span>
          <label class="filter-toggle">
            <input type="checkbox" bind:checked={showVariations} />
            <span class="filter-pill">Variations</span>
          </label>
          <label class="filter-toggle">
            <input type="checkbox" bind:checked={showAdditions} />
            <span class="filter-pill">Additions</span>
          </label>
        </div>
        <input
          class="field"
          bind:value={query}
          placeholder="Search moves, topics, tags, descriptions"
          aria-label="Search moves"
        />
        <div class="dashboard-stats">
          <span>{data.manifest.counts.moveRows} moves</span>
          <span>{data.manifest.counts.trackableMoves} tracked</span>
          <span>{data.manifest.counts.movesWithLocalVideo} videos</span>
        </div>
      </div>
    </div>

    {#if visibleLayoutWithOverflow.length}
      <div class="dashboard-columns">
        {#each visibleLayoutWithOverflow as column, idx (column.isOverflowColumn ? `overflow-${column.column}-${idx}` : column.column)}
          <div class="dashboard-column" class:overflow-column={column.isOverflowColumn}>
            <div
              class="dashboard-column-content"
              class:collapsed={columnMetrics[column.column]?.overflowing && !expandedColumns[column.column] && !column.isOverflowColumn}
              data-column={column.column}
              style={columnMetrics[column.column]?.overflowing && !column.isOverflowColumn ? `--dashboard-column-max-height: ${columnMetrics[column.column]?.maxHeight}px` : undefined}
            >
              {#each column.entries as entry, entryIdx (entry.layoutOrder ?? entry.id ?? entry.name)}
                {#if !column.isOverflowColumn && columnMetrics[column.column]?.overflowing && entryIdx >= (columnMetrics[column.column]?.cutoffIndex ?? column.entries.length)}
                  <!-- Skip entries beyond cutoff in overflowing non-overflow columns -->
                {:else if entry.entryType === 'Title'}
                  <div class="dashboard-row title-row">
                    <div class="title-cell">{entry.name}</div>
                  </div>
                {:else}
                  <a
                    class={`dashboard-row data-row ${typeClass(entry.type)}`}
                    class:preview-selected={selectedPreviewMoveId === entry.id}
                    href={entry.slug ? `/moves/${entry.slug}` : '#'}
                    on:pointerenter={(event) => scheduleHoveredPreview(event, entry)}
                    on:mouseenter={(event) => scheduleHoveredPreview(event, entry)}
                    on:pointerleave={clearHoveredPreview}
                    on:mouseleave={clearHoveredPreview}
                    on:focus={(event) => updateHoveredPreview(event, entry)}
                    on:blur={clearHoveredPreview}
                  >
                    <div class="level-cell">{entry.level ?? ''}</div>
                    <div class={`name-cell ${isEmphasis(entry.type) ? 'emphasis' : ''}`}>{entry.name}</div>
                  </a>
                {/if}
              {/each}
            </div>
            {#if columnMetrics[column.column]?.overflowing && !column.isOverflowColumn && !expandedColumns[column.column]}
              <button type="button" class="column-more" on:click={() => toggleColumn(column.column)}>
                See more
              </button>
            {/if}
            {#if column.isOverflowColumn}
              <button type="button" class="column-more" on:click={() => toggleColumn(column.overflowSourceColumn)}>
                See less
              </button>
            {/if}
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
    {:else}
      <p class="muted">No moves matched.</p>
    {/if}
  </section>
</div>

<style>
  .data-row.preview-selected {
    position: relative;
    z-index: 1;
    outline: 2px solid #204b72;
    outline-offset: -2px;
    box-shadow: 0 0 0 1px rgba(32, 75, 114, 0.18);
    animation: move-row-selected 180ms ease-out;
  }

  @keyframes move-row-selected {
    0% {
      outline-color: rgba(32, 75, 114, 0.35);
      box-shadow: 0 0 0 0 rgba(32, 75, 114, 0);
    }

    45% {
      outline-color: #173f63;
      box-shadow: 0 0 0 2px rgba(32, 75, 114, 0.2);
    }

    100% {
      outline-color: #204b72;
      box-shadow: 0 0 0 1px rgba(32, 75, 114, 0.18);
    }
  }
</style>
