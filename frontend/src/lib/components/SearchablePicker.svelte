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

  const dispatch = createEventDispatcher<{
    select: { option: SearchablePickerOption; id: string };
    remove: { id: string };
    query: { query: string };
    focus: Record<string, never>;
  }>();

  let activeSuggestionIndex = 0;
  let lastQuery = '';
  let inputWrapElement: HTMLDivElement;
  let floatingDropdownStyle = '';

  $: normalizedQuery = normalizeSearchText(query);
  $: unavailableIds = new Set([...selectedIds, ...excludedIds].map(normalizeId));
  $: matches = searchOptions(options, normalizedQuery, unavailableIds, limit);
  $: suggestionResults = matches.results;
  $: hasQuery = normalizedQuery.length > 0;
  $: hasMoreSuggestions = matches.total > suggestionResults.length;
  $: hasVisibleSelection = showSelected && selectedIds.length > 0;
  $: inputPlaceholder = hasVisibleSelection ? addPlaceholder : placeholder;
  $: useFloatingDropdown = floatingDropdown || selectedPlacement === 'inside';

  $: if (query !== lastQuery) {
    lastQuery = query;
    activeSuggestionIndex = 0;
  }

  $: if (activeSuggestionIndex >= suggestionResults.length) {
    activeSuggestionIndex = Math.max(0, suggestionResults.length - 1);
  }

  $: if (useFloatingDropdown && hasQuery) {
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
    dispatch('query', { query });
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
    floatingDropdownStyle = [
      'position: fixed',
      `top: ${rect.bottom + 4}px`,
      `left: ${rect.left}px`,
      `width: ${rect.width}px`
    ].join('; ');
  }

  function selectOption(id: string) {
    const option = findOption(id);
    if (!option || selectedIds.includes(option.id) || excludedIds.includes(option.id)) {
      return;
    }

    dispatch('select', { option, id: option.id });
    setQuery('');
  }

  function removeOption(id: string) {
    dispatch('remove', { id });
  }

  function handleKeydown(event: KeyboardEvent) {
    if ((event.key === 'ArrowDown' || event.key === 'ArrowUp') && hasQuery && suggestionResults.length) {
      event.preventDefault();
      const delta = event.key === 'ArrowDown' ? 1 : -1;
      activeSuggestionIndex = (activeSuggestionIndex + delta + suggestionResults.length) % suggestionResults.length;
      return;
    }

    if (event.key === 'Enter') {
      event.preventDefault();
      if (suggestionResults.length) {
        selectOption(suggestionResults[activeSuggestionIndex]?.id ?? suggestionResults[0].id);
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
        <button type="button" class="shared-chip" aria-label={`Remove ${selectedLabel(id)}`} on:mousedown|preventDefault on:click={() => removeOption(id)}>
          {selectedLabel(id)} x
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
        <button type="button" class="shared-chip searchable-picker-inline-chip" aria-label={`Remove ${selectedLabel(id)}`} on:mousedown|preventDefault on:click={() => removeOption(id)}>
          {selectedLabel(id)} x
        </button>
      {/each}
    {/if}

    <input
      value={query}
      placeholder={inputPlaceholder}
      aria-label={hasVisibleSelection ? addPlaceholder : ariaLabel}
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      {disabled}
      on:focus={() => {
        dispatch('focus', {});
        void scheduleFloatingDropdownUpdate();
      }}
      on:input={(event) => setQuery((event.currentTarget as HTMLInputElement).value)}
      on:keydown={handleKeydown}
    />

    {#if hasQuery}
      <div
        class="searchable-picker-dropdown"
        class:searchable-picker-dropdown-floating={useFloatingDropdown}
        style={useFloatingDropdown ? floatingDropdownStyle : undefined}
        role="listbox"
        aria-label="Search matches"
      >
        {#if suggestionResults.length}
          {#each suggestionResults as option, index}
            <button
              type="button"
              role="option"
              class="searchable-picker-option"
              class:active={index === activeSuggestionIndex}
              aria-selected={index === activeSuggestionIndex}
              on:mousedown|preventDefault
              on:click={() => selectOption(option.id)}
              on:mouseenter={() => (activeSuggestionIndex = index)}
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
        <button type="button" class="shared-chip" aria-label={`Remove ${selectedLabel(id)}`} on:mousedown|preventDefault on:click={() => removeOption(id)}>
          {selectedLabel(id)} x
        </button>
      {/each}
    </div>
  {/if}
</div>
