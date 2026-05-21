<script lang="ts">
  import type { LayoutColumn } from '$lib/types';

  export let title: string;
  export let description: string | null = null;
  export let layout: LayoutColumn[] = [];
  export let moveCount = 0;
  export let backlinkHref = '/moves/create/metadata';
  export let backlinkLabel = 'Back to metadata';

  $: orderedLayout = layout
    .map((column) => ({
      ...column,
      entries: [...column.entries].sort((a, b) => (a.layoutOrder ?? 0) - (b.layoutOrder ?? 0))
    }))
    .sort((a, b) => a.column - b.column);

  function isEmphasis(type: string | null) {
    return type === 'Addition' || type === 'Variation';
  }

  function typeClass(type: string | null) {
    if (type === 'Addition') return 'addition';
    if (type === 'Variation') return 'variation';
    return '';
  }
</script>

<div class="stack metadata-splash-page">
  <a class="pill move-backlink" href={backlinkHref}>{backlinkLabel}</a>

  <section class="dashboard-page metadata-splash-dashboard">
    <div class="dashboard-titlebar metadata-splash-titlebar">
      <div>
        <h1>{title}</h1>
        {#if description}
          <p class="muted">{description}</p>
        {/if}
      </div>
      <div class="dashboard-stats">
        <span>{moveCount} moves</span>
      </div>
    </div>

    {#if orderedLayout.length}
      <div class="dashboard-columns">
        {#each orderedLayout as column}
          <div class="dashboard-column">
            <div class="dashboard-column-content">
              {#each column.entries as entry (entry.layoutOrder ?? entry.id ?? entry.name)}
                {#if entry.entryType === 'Title'}
                  <div class="dashboard-row title-row">
                    <div class="title-cell">{entry.name}</div>
                  </div>
                {:else}
                  <a class={`dashboard-row data-row ${typeClass(entry.type)}`} href={entry.slug ? `/moves/${entry.slug}` : '#'}>
                    <div class="level-cell">{entry.level ?? ''}</div>
                    <div class={`name-cell ${isEmphasis(entry.type) ? 'emphasis' : ''}`}>{entry.name}</div>
                  </a>
                {/if}
              {/each}
            </div>
          </div>
        {/each}
      </div>
    {:else}
      <p class="muted">No moves are linked to this metadata entry yet.</p>
    {/if}
  </section>
</div>
