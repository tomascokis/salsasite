<script lang="ts">
  import ContentBadge from './ContentBadge.svelte';
  import type { ContentStatusKey } from '$lib/content-status';

  type EditableListBadge = {
    label?: string;
    status?: ContentStatusKey;
  };

  type EditableListItem = {
    id: string;
    title: string;
    description?: string | null;
    meta?: string | null;
    badges?: EditableListBadge[];
  };

  export let items: EditableListItem[] = [];
  export let emptyText = 'No entries yet.';
  export let editLabel = 'Edit';
  export let removeLabel = 'Remove';
  export let showEdit = true;
  export let showRemove = false;
</script>

<div class="editable-list">
  {#if items.length}
    {#each items as item (item.id)}
      <article class="editable-list-row">
        <div class="editable-list-main">
          <div class="editable-list-title-row">
            <strong>{item.title}</strong>
            {#if item.badges?.length}
              <span class="content-badge-list">
                {#each item.badges as badge}
                  <ContentBadge label={badge.label ?? ''} status={badge.status ?? null} />
                {/each}
              </span>
            {/if}
          </div>
          {#if item.description}
            <p>{item.description}</p>
          {/if}
          {#if item.meta}
            <span class="editable-list-meta">{item.meta}</span>
          {/if}
        </div>
        <div class="editable-list-actions">
          {#if showEdit}
            <slot name="edit" {item}>
              <button type="button">{editLabel}</button>
            </slot>
          {/if}
          {#if showRemove}
            <slot name="remove" {item}>
              <button type="button">{removeLabel}</button>
            </slot>
          {/if}
        </div>
      </article>
    {/each}
  {:else}
    <p class="muted editable-list-empty">{emptyText}</p>
  {/if}
</div>
