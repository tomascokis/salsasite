<script lang="ts">
  import { colorForProgress } from '$lib/progress-ui';
  import type { ProgressView } from '$lib/types';

  export let data: {
    views: ProgressView[];
  };

  let activeIndex = 0;

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
</script>

<div class="stack">
  <section class="panel">
    <div class="panel-header">
      <div class="panel-heading-row">
        <h2 class="sr-only">Progress</h2>
        <a class="header-button" href="/progress/editor">Log new progress</a>
      </div>
    </div>
    <div style="padding: 1rem 1.1rem" class="stack">
      <div class="snapshot-tabs">
        {#each data.views as view, index}
          <button class:active={activeIndex === index} on:click={() => (activeIndex = index)}>
            {view.label}
          </button>
        {/each}
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
                    href={`/moves/${entry.slug}`}
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
      {/if}
    </div>
  </section>
</div>
