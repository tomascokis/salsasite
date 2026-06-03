<script lang="ts">
  import { createEventDispatcher, tick } from 'svelte';

  type SearchablePickerOption = {
    id: string;
    label: string;
    secondary?: string | null;
    imageUrl?: string | null;
  };

  export let options: SearchablePickerOption[] = [];
  export let selectedIds: string[] = [];
  export let excludedIds: string[] = [];
  export let query = '';
  export let limit = 8;
  export let placeholder = 'Search';
  export let addPlaceholder = 'Add another';
  export let ariaLabel = 'Search';
  export let showSelected = true;
  export let selectedPlacement: 'before' | 'after' | 'inside' = 'before';
  export let floatingDropdown = false;
  export let disabled = false;
  export let emptyText = 'No matches';
  export let moreText = 'and more...';
  export let allowCreate = false;
  export let createLabel = 'Use';
  export let maxSelected: number | null = null;
  export let clearQueryOnSelect = true;
  export let onselect: ((detail: { option: SearchablePickerOption; id: string }) => void) | undefined = undefined;
  export let onremove: ((detail: { id: string }) => void) | undefined = undefined;
  export let onquery: ((detail: { query: string }) => void) | undefined = undefined;
  export let onfocus: ((detail: Record<string, never>) => void) | undefined = undefined;
  export let oncreate: ((detail: { value: string }) => void) | undefined = undefined;

  const dispatch = createEventDispatcher<{
    select: { option: SearchablePickerOption; id: string };
    remove: { id: string };
    query: { query: string };
    focus: Record<string, never>;
    create: { value: string };
  }>();

  let activeSuggestionIndex = 0;
  let lastQuery = '';
  let inputWrapElement: HTMLDivElement;
  let dropdownElement: HTMLDivElement;
  let floatingDropdownStyle = '';

  $: normalizedQuery = normalizeSearchText(query);
  $: unavailableIds = new Set([...selectedIds, ...excludedIds].map(normalizeId));
  $: matches = searchOptions(options, normalizedQuery, unavailableIds, limit);
  $: suggestionResults = matches.results;
  $: hasQuery = normalizedQuery.length > 0;
  $: hasExactAvailableMatch = options.some(
    (option) => normalizeId(option.id) === normalizeId(query) || normalizeId(option.label) === normalizeId(query)
  );
  $: canCreate = allowCreate && hasQuery && !hasExactAvailableMatch && !unavailableIds.has(normalizeId(query));
  $: suggestionIndexOffset = canCreate ? 1 : 0;
  $: suggestionCount = suggestionResults.length + suggestionIndexOffset;
  $: hasMoreSuggestions = matches.total > suggestionResults.length;
  $: hasVisibleSelection = showSelected && selectedIds.length > 0;
  $: selectionFull = maxSelected !== null && selectedIds.length >= maxSelected;
  $: inputPlaceholder = hasVisibleSelection ? addPlaceholder : placeholder;
  $: inputAriaLabel = hasVisibleSelection && addPlaceholder ? addPlaceholder : ariaLabel;
  $: useFloatingDropdown = floatingDropdown || selectedPlacement === 'inside';

  $: if (query !== lastQuery) {
    lastQuery = query;
    activeSuggestionIndex = canCreate && suggestionResults.length ? 1 : 0;
  }

  $: if (activeSuggestionIndex >= suggestionCount) {
    activeSuggestionIndex = Math.max(0, suggestionCount - 1);
  }

  $: if (useFloatingDropdown && hasQuery && !selectionFull) {
    void scheduleFloatingDropdownUpdate();
  }

  function normalizeId(value: string) {
    return value.trim().toLocaleLowerCase();
  }

  function normalizeSearchText(value: string | null | undefined) {
    return String(value ?? '')
      .toLocaleLowerCase()
      .replace(/[_:\/-]+/g, ' ')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function tokenizeSearchText(value: string) {
    return normalizeSearchText(value).split(' ').filter(Boolean);
  }

  function scoreField(value: string, queryText: string, queryTokens: string[], weight: number) {
    if (!value) return 0;
    const paddedValue = ` ${value} `;
    const paddedQuery = ` ${queryText} `;

    if (value === queryText) return weight + 1000;
    if (value.startsWith(`${queryText} `)) return weight + 800 - value.length / 100;
    if (paddedValue.includes(paddedQuery)) return weight + 650 - value.indexOf(queryText) / 100;
    if (value.includes(queryText)) return weight + 450 - value.indexOf(queryText) / 100;
    if (queryTokens.length && queryTokens.every((token) => value.includes(token))) {
      const positions = queryTokens.map((token) => value.indexOf(token)).filter((position) => position >= 0);
      const spread = Math.max(...positions) - Math.min(...positions);
      return weight + 240 - spread / 100;
    }
    return 0;
  }

  function scoreOption(option: SearchablePickerOption, queryText: string, queryTokens: string[]) {
    return Math.max(
      scoreField(normalizeSearchText(option.label), queryText, queryTokens, 100),
      scoreField(normalizeSearchText(option.id), queryText, queryTokens, 70),
      scoreField(normalizeSearchText(option.secondary), queryText, queryTokens, 50)
    );
  }

  function searchOptions(
    availableOptions: SearchablePickerOption[],
    queryText: string,
    unavailableOptionIds: Set<string>,
    maxResults: number
  ) {
    if (!queryText) {
      return { results: [], total: 0 };
    }

    const queryTokens = tokenizeSearchText(queryText);
    const scored = availableOptions
      .map((option, index) => ({ option, index, score: scoreOption(option, queryText, queryTokens) }))
      .filter((entry) => entry.score > 0 && !unavailableOptionIds.has(normalizeId(entry.option.id)))
      .sort((left, right) => {
        if (right.score !== left.score) return right.score - left.score;
        return left.option.label.localeCompare(right.option.label, undefined, { sensitivity: 'base' }) || left.index - right.index;
      });

    return {
      results: scored.slice(0, maxResults).map((entry) => entry.option),
      total: scored.length
    };
  }

  function findOption(id: string) {
    const normalized = normalizeId(id);
    return options.find((option) => normalizeId(option.id) === normalized) ?? null;
  }

  function selectedLabel(id: string) {
    const option = findOption(id);
    return option?.label ?? id;
  }

  function setQuery(value: string) {
    query = value;
    const detail = { query };
    dispatch('query', detail);
    onquery?.(detail);
    void scheduleFloatingDropdownUpdate();
  }

  async function scheduleFloatingDropdownUpdate() {
    if (!useFloatingDropdown) return;
    await tick();
    updateFloatingDropdown();
  }

  function updateFloatingDropdown() {
    if (!useFloatingDropdown || !inputWrapElement) return;
    const rect = inputWrapElement.getBoundingClientRect();
    const gap = 4;
    const viewportPadding = 8;
    const maxDropdownHeight = 352;
    const availableBelow = window.innerHeight - rect.bottom - viewportPadding;
    const availableAbove = rect.top - viewportPadding;
    const openAbove = availableBelow < 220 && availableAbove > availableBelow;
    const availableHeight = Math.max(80, Math.min(maxDropdownHeight, openAbove ? availableAbove - gap : availableBelow - gap));
    floatingDropdownStyle = [
      'position: fixed',
      `left: ${rect.left}px`,
      `width: ${rect.width}px`,
      `max-height: ${availableHeight}px`,
      openAbove ? 'top: auto' : 'bottom: auto',
      openAbove ? `bottom: ${window.innerHeight - rect.top + gap}px` : `top: ${rect.bottom + gap}px`
    ].join('; ');
  }

  async function scrollActiveSuggestionIntoView() {
    await tick();
    if (!dropdownElement) return;

    const activeOption = dropdownElement.querySelector<HTMLElement>('[aria-selected="true"]');
    if (!activeOption) return;

    const activeTop = activeOption.offsetTop;
    const activeBottom = activeTop + activeOption.offsetHeight;
    const visibleTop = dropdownElement.scrollTop;
    const visibleBottom = visibleTop + dropdownElement.clientHeight;

    if (activeTop < visibleTop) {
      dropdownElement.scrollTop = activeTop;
    } else if (activeBottom > visibleBottom) {
      dropdownElement.scrollTop = activeBottom - dropdownElement.clientHeight;
    }
  }

  function selectOption(id: string) {
    const option = findOption(id);
    if (!option || selectionFull || selectedIds.includes(option.id) || excludedIds.includes(option.id)) {
      return;
    }

    const detail = { option, id: option.id };
    dispatch('select', detail);
    onselect?.(detail);
    if (clearQueryOnSelect) {
      setQuery('');
    }
  }

  function createOption() {
    const value = query.trim();
    if (!value || !allowCreate || selectionFull) {
      return;
    }

    const detail = { value };
    dispatch('create', detail);
    oncreate?.(detail);
    setQuery('');
  }

  function removeOption(id: string) {
    const detail = { id };
    dispatch('remove', detail);
    onremove?.(detail);
  }

  function handleRemoveKeydown(event: KeyboardEvent, id: string) {
    if (event.key !== 'Enter' && event.key !== ' ') return;
    event.preventDefault();
    removeOption(id);
  }

  function handleKeydown(event: KeyboardEvent) {
    if ((event.key === 'ArrowDown' || event.key === 'ArrowUp') && hasQuery && !selectionFull) {
      event.preventDefault();
      if (!suggestionCount) {
        return;
      }
      const delta = event.key === 'ArrowDown' ? 1 : -1;
      activeSuggestionIndex = (activeSuggestionIndex + delta + suggestionCount) % suggestionCount;
      void scrollActiveSuggestionIntoView();
      return;
    }

    if (event.key === 'Enter') {
      event.preventDefault();
      if (selectionFull) {
        return;
      }

      if (canCreate && activeSuggestionIndex === 0) {
        createOption();
        return;
      }

      const suggestionIndex = activeSuggestionIndex - suggestionIndexOffset;
      if (suggestionResults.length) {
        selectOption(suggestionResults[suggestionIndex]?.id ?? suggestionResults[0].id);
      } else if (canCreate) {
        createOption();
      }
      return;
    }

    if (event.key === 'Backspace' && !query && hasVisibleSelection) {
      removeOption(selectedIds[selectedIds.length - 1]);
      return;
    }

    if (event.key === 'Escape' && query) {
      event.preventDefault();
      setQuery('');
    }
  }
</script>

<svelte:window on:resize={updateFloatingDropdown} on:scroll={updateFloatingDropdown} />

<div class="searchable-picker">
  {#if selectedPlacement === 'before' && showSelected && selectedIds.length}
    <div class="shared-chip-row">
      {#each selectedIds as id}
        <button type="button" class="shared-chip" aria-label={`Remove ${selectedLabel(id)}`} onpointerdown={(event) => { event.preventDefault(); removeOption(id); }} onkeydown={(event) => handleRemoveKeydown(event, id)}>
          <span class="picker-chip-label">{selectedLabel(id)}</span>
          <span class="picker-chip-remove" aria-hidden="true"></span>
        </button>
      {/each}
    </div>
  {/if}

  <div
    class="searchable-picker-input-wrap"
    class:searchable-picker-input-inline={selectedPlacement === 'inside'}
    bind:this={inputWrapElement}
  >
    {#if selectedPlacement === 'inside' && showSelected && selectedIds.length}
      {#each selectedIds as id}
        <button type="button" class="shared-chip searchable-picker-inline-chip" aria-label={`Remove ${selectedLabel(id)}`} onpointerdown={(event) => { event.preventDefault(); removeOption(id); }} onkeydown={(event) => handleRemoveKeydown(event, id)}>
          <span class="picker-chip-label">{selectedLabel(id)}</span>
          <span class="picker-chip-remove" aria-hidden="true"></span>
        </button>
      {/each}
    {/if}

    {#if !selectionFull}
      <input
        value={query}
        placeholder={inputPlaceholder}
        aria-label={inputAriaLabel}
        autocomplete="off"
        autocorrect="off"
        spellcheck="false"
        {disabled}
        onfocus={() => {
          dispatch('focus', {});
          onfocus?.({});
          void scheduleFloatingDropdownUpdate();
        }}
        oninput={(event) => setQuery((event.currentTarget as HTMLInputElement).value)}
        onkeydown={handleKeydown}
      />
    {/if}

    {#if hasQuery && !selectionFull}
      <div
        class="searchable-picker-dropdown"
        class:searchable-picker-dropdown-floating={useFloatingDropdown}
        style={useFloatingDropdown ? floatingDropdownStyle : undefined}
        role="listbox"
        aria-label="Search matches"
        bind:this={dropdownElement}
      >
        {#if suggestionResults.length || canCreate}
          {#if canCreate}
            <button
              type="button"
              role="option"
              class="searchable-picker-option searchable-picker-create-option"
              class:active={activeSuggestionIndex === 0}
              aria-selected={activeSuggestionIndex === 0}
              onpointerdown={(event) => { event.preventDefault(); createOption(); }}
              onmouseenter={() => (activeSuggestionIndex = 0)}
            >
              <span class="searchable-picker-option-text">
                <strong>{createLabel}</strong>
                <span>{query.trim()}</span>
              </span>
            </button>
          {/if}
          {#each suggestionResults as option, index}
            <button
              type="button"
              role="option"
              class="searchable-picker-option"
              class:active={index + suggestionIndexOffset === activeSuggestionIndex}
              aria-selected={index + suggestionIndexOffset === activeSuggestionIndex}
              onpointerdown={(event) => { event.preventDefault(); selectOption(option.id); }}
              onmouseenter={() => (activeSuggestionIndex = index + suggestionIndexOffset)}
            >
              {#if option.imageUrl}
                <span class="searchable-picker-option-image" aria-hidden="true">
                  <img src={option.imageUrl} alt="" loading="lazy" />
                </span>
              {/if}
              <span class="searchable-picker-option-text">
                <strong>{option.label}</strong>
                {#if option.secondary}
                  <span>{option.secondary}</span>
                {/if}
              </span>
            </button>
          {/each}
          {#if hasMoreSuggestions}
            <span class="searchable-picker-more">{moreText}</span>
          {/if}
        {:else}
          <span class="searchable-picker-empty">{emptyText}</span>
        {/if}
      </div>
    {/if}
  </div>

  {#if selectedPlacement === 'after' && showSelected && selectedIds.length}
    <div class="shared-chip-row">
      {#each selectedIds as id}
        <button type="button" class="shared-chip" aria-label={`Remove ${selectedLabel(id)}`} onpointerdown={(event) => { event.preventDefault(); removeOption(id); }} onkeydown={(event) => handleRemoveKeydown(event, id)}>
          <span class="picker-chip-label">{selectedLabel(id)}</span>
          <span class="picker-chip-remove" aria-hidden="true"></span>
        </button>
      {/each}
    </div>
  {/if}
</div>
