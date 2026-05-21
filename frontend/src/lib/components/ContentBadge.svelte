<script lang="ts">
  import {
    contentStatusDefinition,
    type ContentStatusKey,
    type ContentStatusTone
  } from '$lib/content-status';

  export let label = '';
  export let status: ContentStatusKey | null = null;
  export let tone: ContentStatusTone = 'neutral';
  export let title: string | null = null;
  export let className = '';

  $: definition = status ? contentStatusDefinition(status) : null;
  $: badgeLabel = label || definition?.label || '';
  $: resolvedTone = definition?.tone ?? tone;
  $: resolvedTitle = title ?? definition?.description ?? undefined;
</script>

{#if badgeLabel}
  <span class={`content-badge content-badge-${resolvedTone} ${className}`} title={resolvedTitle}>
    {badgeLabel}
  </span>
{/if}
