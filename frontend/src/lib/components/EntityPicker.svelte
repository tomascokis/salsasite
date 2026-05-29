<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import type {
    EntityPickerMoveOption,
    EntityPickerSearchOption,
    EntityPickerTemplate
  } from '$lib/components/entity-picker';

  export let template: EntityPickerTemplate;
  export let options: Array<EntityPickerSearchOption | EntityPickerMoveOption> = [];
  export let selectedIds: string[] = [];
  export let excludedIds: string[] = [];
  export let query = '';
  export let limit = 8;
  export let disabled = false;

  const dispatch = createEventDispatcher<{
    select: {
      id: string;
      option?: EntityPickerSearchOption;
      move?: EntityPickerMoveOption;
    };
    remove: { id: string };
    query: { query: string };
    focus: Record<string, never>;
    create: { value: string };
  }>();

  $: searchableOptions = options as EntityPickerSearchOption[];
  $: moveOptions = options as EntityPickerMoveOption[];
</script>

<div class="entity-picker-field">
  <div class="entity-picker-field-header">
    <h3>{template.title}</h3>
    <p>{template.description}</p>
  </div>

  {#if template.kind === 'move'}
    <MovePicker
      moves={moveOptions}
      {selectedIds}
      {excludedIds}
      {query}
      {limit}
      placeholder={template.placeholder}
      addPlaceholder={template.addPlaceholder}
      ariaLabel={template.ariaLabel}
      showSelected={template.showSelected ?? true}
      selectedPlacement={template.selectedPlacement ?? 'inside'}
      floatingDropdown={template.floatingDropdown ?? true}
      disabled={disabled}
      emptyText={template.emptyText ?? 'No matching moves'}
      moreText={template.moreText ?? 'and more...'}
      allowCreate={template.allowCreate ?? false}
      createText={template.createText ?? 'Create new move'}
      showId={template.showId ?? true}
      showName={template.showName ?? true}
      showPoster={template.showPoster ?? false}
      on:query={(event) => dispatch('query', { query: event.detail.query })}
      on:focus={() => dispatch('focus', {})}
      on:select={(event) => dispatch('select', { id: event.detail.moveId, move: event.detail.move })}
      on:remove={(event) => dispatch('remove', { id: event.detail.moveId })}
      on:create={(event) => dispatch('create', { value: event.detail.query })}
    />
  {:else}
    <SearchablePicker
      options={searchableOptions}
      {selectedIds}
      {excludedIds}
      {query}
      {limit}
      placeholder={template.placeholder}
      addPlaceholder={template.addPlaceholder}
      ariaLabel={template.ariaLabel}
      showSelected={template.showSelected ?? true}
      selectedPlacement={template.selectedPlacement ?? 'inside'}
      floatingDropdown={template.floatingDropdown ?? true}
      disabled={disabled}
      emptyText={template.emptyText ?? 'No matches'}
      moreText={template.moreText ?? 'and more...'}
      allowCreate={template.allowCreate ?? false}
      createLabel={template.createLabel ?? 'Use'}
      on:query={(event) => dispatch('query', { query: event.detail.query })}
      on:focus={() => dispatch('focus', {})}
      on:select={(event) => dispatch('select', { id: event.detail.id, option: event.detail.option })}
      on:remove={(event) => dispatch('remove', { id: event.detail.id })}
      on:create={(event) => dispatch('create', { value: event.detail.value })}
    />
  {/if}
</div>
