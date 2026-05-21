<script lang="ts">
  import OverviewSplash from '$lib/components/OverviewSplash.svelte';
  import type { LayoutColumn, MetadataEntry } from '$lib/types';

  export let data: {
    entry: MetadataEntry;
    layout: LayoutColumn[];
  };
</script>

<svelte:head>
  <title>{data.entry.name} | Salsa Encyclopedia</title>
</svelte:head>

<OverviewSplash
  title={data.entry.name}
  description={data.entry.description}
  layout={data.layout}
  moveCount={data.entry.moveCount}
/>
