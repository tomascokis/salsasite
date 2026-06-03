<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import type {
    EntityPickerMoveOption,
    EntityPickerSearchOption,
    EntityPickerTemplate,
    EntityPickerCreatePolicy,
    EntityPickerMode,
    EntityPickerValueSource
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
  $: resolved = resolveTemplate(template);
  $: pickerSelectedIds = selectedIds.map((id) => optionIdForValue(id, resolved.valueSource)).filter(Boolean);

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

  function resolveTemplate(pickerTemplate: EntityPickerTemplate) {
    const mode: EntityPickerMode = pickerTemplate.mode ?? 'singleEdit';
    const createPolicy: EntityPickerCreatePolicy =
      pickerTemplate.createPolicy ?? (pickerTemplate.allowCreate ? 'local' : 'none');
    const valueSource: EntityPickerValueSource = pickerTemplate.valueSource ?? 'id';
    const modeDefaults = {
      showSelected: mode !== 'browse',
      selectedPlacement: mode === 'browse' ? 'before' : 'inside',
      floatingDropdown: true,
      maxSelected: mode === 'singleEdit' || mode === 'strict' ? 1 : null,
      allowCreate: createPolicy !== 'none' && mode !== 'browse' && mode !== 'filter' && mode !== 'strict'
    };

    return {
      mode,
      createPolicy,
      valueSource,
      density: pickerTemplate.density ?? 'default',
      showSelected: pickerTemplate.showSelected ?? modeDefaults.showSelected,
      selectedPlacement: pickerTemplate.selectedPlacement ?? modeDefaults.selectedPlacement,
      floatingDropdown: pickerTemplate.floatingDropdown ?? modeDefaults.floatingDropdown,
      maxSelected: pickerTemplate.maxSelected ?? modeDefaults.maxSelected,
      allowCreate: pickerTemplate.allowCreate ?? modeDefaults.allowCreate
    };
  }

  function searchableOptionForValue(value: string) {
    if (resolved.valueSource === 'label') {
      return searchableOptions.find((option) => option.label === value || option.id === value) ?? null;
    }
    return searchableOptions.find((option) => option.id === value) ?? null;
  }

  function moveOptionForValue(value: string) {
    return moveOptions.find((move) => move.id === value || move.displayId === value || move.name === value) ?? null;
  }

  function optionIdForValue(value: string, valueSource: EntityPickerValueSource) {
    if (template.kind === 'move') {
      return moveOptionForValue(value)?.id ?? value;
    }
    if (valueSource === 'label') {
      return searchableOptionForValue(value)?.id ?? value;
    }
    return value;
  }

  function searchValueForOption(option: EntityPickerSearchOption) {
    return resolved.valueSource === 'label' ? option.label : option.id;
  }

  function moveValueForOption(move: EntityPickerMoveOption) {
    if (resolved.valueSource === 'label') {
      return move.name ?? move.displayId ?? move.id;
    }
    return move.id;
  }

  function removeValueForId(id: string) {
    if (template.kind === 'move') {
      const move = moveOptionForValue(id);
      return move ? moveValueForOption(move) : id;
    }
    const option = searchableOptions.find((candidate) => candidate.id === id);
    return option ? searchValueForOption(option) : id;
  }
</script>

<div class="entity-picker-field" class:entity-picker-field-compact={resolved.density === 'compact'}>
  <div class="entity-picker-field-header">
    <h3>{template.title}</h3>
    <p>{template.description}</p>
  </div>

  {#if template.kind === 'move'}
    <MovePicker
      moves={moveOptions}
      selectedIds={pickerSelectedIds}
      {excludedIds}
      {query}
      limit={template.limit ?? limit}
      placeholder={template.placeholder}
      addPlaceholder={template.addPlaceholder}
      ariaLabel={template.ariaLabel}
      showSelected={resolved.showSelected}
      selectedPlacement={resolved.selectedPlacement}
      floatingDropdown={resolved.floatingDropdown}
      disabled={disabled}
      maxSelected={resolved.maxSelected}
      emptyText={template.emptyText ?? 'No matching moves'}
      moreText={template.moreText ?? 'and more...'}
      allowCreate={resolved.allowCreate}
      createText={template.createText ?? 'Create new move'}
      showId={template.showId ?? true}
      showName={template.showName ?? true}
      showPoster={template.showPoster ?? false}
      onquery={(detail) => emitQuery({ query: detail.query })}
      onfocus={emitFocus}
      onselect={(detail) => emitSelect({ id: moveValueForOption(detail.move), move: detail.move })}
      onremove={(detail) => emitRemove({ id: removeValueForId(detail.moveId) })}
      oncreate={(detail) => emitCreate({ value: detail.query })}
    />
  {:else}
    <SearchablePicker
      options={searchableOptions}
      selectedIds={pickerSelectedIds}
      {excludedIds}
      {query}
      limit={template.limit ?? limit}
      placeholder={template.placeholder}
      addPlaceholder={template.addPlaceholder}
      ariaLabel={template.ariaLabel}
      showSelected={resolved.showSelected}
      selectedPlacement={resolved.selectedPlacement}
      floatingDropdown={resolved.floatingDropdown}
      disabled={disabled}
      maxSelected={resolved.maxSelected}
      emptyText={template.emptyText ?? 'No matches'}
      moreText={template.moreText ?? 'and more...'}
      allowCreate={resolved.allowCreate}
      createLabel={template.createLabel ?? 'Use'}
      onquery={(detail) => emitQuery({ query: detail.query })}
      onfocus={emitFocus}
      onselect={(detail) => emitSelect({ id: searchValueForOption(detail.option), option: detail.option })}
      onremove={(detail) => emitRemove({ id: removeValueForId(detail.id) })}
      oncreate={(detail) => emitCreate({ value: detail.value })}
    />
  {/if}
</div>
