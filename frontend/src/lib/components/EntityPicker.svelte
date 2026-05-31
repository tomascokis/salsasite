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
  export let onselect: ((detail: { id: string; option?: EntityPickerSearchOption; move?: EntityPickerMoveOption }) => void) | undefined = undefined;
  export let onremove: ((detail: { id: string }) => void) | undefined = undefined;
  export let onquery: ((detail: { query: string }) => void) | undefined = undefined;
  export let onfocus: ((detail: Record<string, never>) => void) | undefined = undefined;
  export let oncreate: ((detail: { value: string }) => void) | undefined = undefined;

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

  function emitSelect(detail: { id: string; option?: EntityPickerSearchOption; move?: EntityPickerMoveOption }) {
    dispatch('select', detail);
    onselect?.(detail);
  }

  function emitRemove(detail: { id: string }) {
    dispatch('remove', detail);
    onremove?.(detail);
  }

  function emitQuery(detail: { query: string }) {
    dispatch('query', detail);
    onquery?.(detail);
  }

  function emitFocus() {
    dispatch('focus', {});
    onfocus?.({});
  }

  function emitCreate(detail: { value: string }) {
    dispatch('create', detail);
    oncreate?.(detail);
  }
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
      limit={template.limit ?? limit}
      placeholder={template.placeholder}
      addPlaceholder={template.addPlaceholder}
      ariaLabel={template.ariaLabel}
      showSelected={template.showSelected ?? true}
      selectedPlacement={template.selectedPlacement ?? 'inside'}
      floatingDropdown={template.floatingDropdown ?? true}
      disabled={disabled}
      maxSelected={template.maxSelected ?? null}
      emptyText={template.emptyText ?? 'No matching moves'}
      moreText={template.moreText ?? 'and more...'}
      allowCreate={template.allowCreate ?? false}
      createText={template.createText ?? 'Create new move'}
      showId={template.showId ?? true}
      showName={template.showName ?? true}
      showPoster={template.showPoster ?? false}
      onquery={(detail) => emitQuery({ query: detail.query })}
      onfocus={emitFocus}
      onselect={(detail) => emitSelect({ id: detail.moveId, move: detail.move })}
      onremove={(detail) => emitRemove({ id: detail.moveId })}
      oncreate={(detail) => emitCreate({ value: detail.query })}
    />
  {:else}
    <SearchablePicker
      options={searchableOptions}
      {selectedIds}
      {excludedIds}
      {query}
      limit={template.limit ?? limit}
      placeholder={template.placeholder}
      addPlaceholder={template.addPlaceholder}
      ariaLabel={template.ariaLabel}
      showSelected={template.showSelected ?? true}
      selectedPlacement={template.selectedPlacement ?? 'inside'}
      floatingDropdown={template.floatingDropdown ?? true}
      disabled={disabled}
      maxSelected={template.maxSelected ?? null}
      emptyText={template.emptyText ?? 'No matches'}
      moreText={template.moreText ?? 'and more...'}
      allowCreate={template.allowCreate ?? false}
      createLabel={template.createLabel ?? 'Use'}
      onquery={(detail) => emitQuery({ query: detail.query })}
      onfocus={emitFocus}
      onselect={(detail) => emitSelect({ id: detail.id, option: detail.option })}
      onremove={(detail) => emitRemove({ id: detail.id })}
      oncreate={(detail) => emitCreate({ value: detail.value })}
    />
  {/if}
</div>
