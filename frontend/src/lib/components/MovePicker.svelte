<script lang="ts">
  import { createEventDispatcher, tick } from 'svelte';
  import { moveSuggestionSearch } from '$lib/video-library-utils.js';

  type MoveOption = {
    id: string;
    slug?: string | null;
    name?: string | null;
    posterFile?: string | null;
    thumbnailUrl?: string | null;
    level?: string | null;
    isDraft?: boolean;
  };

  export let moves: MoveOption[] = [];
  export let selectedIds: string[] = [];
  export let excludedIds: string[] = [];
  export let query = '';
  export let limit = 8;
  export let placeholder = 'Search moves by id or name';
  export let addPlaceholder = 'Add another move';
  export let ariaLabel = 'Search moves by id or name';
  export let showSelected = true;
  export let selectedPlacement: 'before' | 'after' | 'inside' = 'before';
  export let showId = true;
  export let showName = true;
  export let showPoster = false;
  export let floatingDropdown = false;
  export let disabled = false;
  export let emptyText = 'No matching moves';
  export let moreText = 'and more...';
  export let allowCreate = false;
  export let createText = 'Create new move';
  export let autocapitalize: 'none' | 'characters' | 'off' | 'on' | 'sentences' | 'words' = 'off';

  const dispatch = createEventDispatcher<{
    select: { move: MoveOption; moveId: string };
    remove: { moveId: string };
    query: { query: string };
    focus: Record<string, never>;
    create: { query: string };
  }>();

  let activeSuggestionIndex = 0;
  let lastQuery = '';
  let inputWrapElement: HTMLDivElement;
  let floatingDropdownStyle = '';

  $: normalizedQuery = query.trim();
  $: excludedMoveIds = [...selectedIds, ...excludedIds].filter(Boolean);
  $: search = moveSuggestionSearch(moves, query, excludedMoveIds, limit);
  $: suggestionResults = search.results;
  $: hasQuery = normalizedQuery.length > 0;
  $: hasCreateOption = allowCreate && hasQuery;
  $: suggestionIndexOffset = hasCreateOption ? 1 : 0;
  $: optionCount = suggestionResults.length + suggestionIndexOffset;
  $: hasMoreSuggestions = search.total > suggestionResults.length;
  $: hasVisibleSelection = showSelected && selectedIds.length > 0;
  $: inputPlaceholder = hasVisibleSelection ? addPlaceholder : placeholder;
  $: useFloatingDropdown = floatingDropdown || selectedPlacement === 'inside';

  $: if (query !== lastQuery) {
    lastQuery = query;
    activeSuggestionIndex = hasCreateOption && suggestionResults.length ? 1 : 0;
  }

  $: if (activeSuggestionIndex >= optionCount) {
    activeSuggestionIndex = Math.max(0, optionCount - 1);
  }

  $: if (useFloatingDropdown && hasQuery) {
    void scheduleFloatingDropdownUpdate();
  }

  function normalizeMoveId(value: string) {
    return value.trim().toUpperCase();
  }

  function findMove(moveId: string) {
    const normalized = normalizeMoveId(moveId);
    return moves.find((move) => normalizeMoveId(move.id) === normalized) ?? null;
  }

  function isDraftMove(move: MoveOption) {
    return Boolean(move.isDraft);
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
    const gap = 4;
    const viewportPadding = 8;
    const maxDropdownHeight = 352;
    const availableBelow = window.innerHeight - rect.bottom - viewportPadding;
    const availableAbove = rect.top - viewportPadding;
    const openAbove = availableBelow < 220 && availableAbove > availableBelow;
    const availableHeight = Math.max(80, Math.min(maxDropdownHeight, openAbove ? availableAbove - gap : availableBelow - gap));
    floatingDropdownStyle = [
      `position: fixed`,
      `left: ${rect.left}px`,
      `width: ${rect.width}px`,
      `max-height: ${availableHeight}px`,
      openAbove ? 'top: auto' : 'bottom: auto',
      openAbove ? `bottom: ${window.innerHeight - rect.top + gap}px` : `top: ${rect.bottom + gap}px`
    ].join('; ');
  }

  function moveLabel(moveId: string) {
    const move = findMove(moveId);
    if (!move) return moveId;
    if (isDraftMove(move)) {
      return showName && move.name ? `[Draft] ${move.name}` : '[Draft]';
    }
    if (showId && showName && move.name) return `${move.id} ${move.name}`;
    if (showName && move.name) return move.name;
    if (showId) return move.id;
    return move.name ?? move.id;
  }

  function primaryLabel(move: MoveOption) {
    if (showName && move.name) return move.name;
    if (showId) return move.id;
    return move.name ?? move.id;
  }

  function secondaryLabel(move: MoveOption) {
    const parts: string[] = [];
    if (isDraftMove(move)) {
      parts.push('[Draft]');
    } else if (showId && primaryLabel(move) !== move.id) {
      parts.push(move.id);
    }
    if (showName && move.name && primaryLabel(move) !== move.name) {
      parts.push(move.name);
    }
    if (move.level) {
      parts.push(`Level ${move.level}`);
    }
    return parts.join(' · ');
  }

  function posterSrc(move: MoveOption) {
    if (move.thumbnailUrl) return move.thumbnailUrl;
    if (move.posterFile) return `/posters/${encodeURIComponent(move.posterFile)}`;
    return '';
  }

  function selectMove(moveId: string) {
    const move = findMove(moveId);
    if (!move || selectedIds.includes(move.id) || excludedIds.includes(move.id)) {
      return;
    }

    dispatch('select', { move, moveId: move.id });
    setQuery('');
  }

  function removeMove(moveId: string) {
    dispatch('remove', { moveId });
  }

  function createMove() {
    if (!hasCreateOption) {
      return;
    }

    dispatch('create', { query: normalizedQuery });
    setQuery('');
  }

  function handleInput(event: Event) {
    setQuery((event.currentTarget as HTMLInputElement).value);
  }

  function handleKeydown(event: KeyboardEvent) {
    if ((event.key === 'ArrowDown' || event.key === 'ArrowUp') && hasQuery && optionCount) {
      event.preventDefault();
      const delta = event.key === 'ArrowDown' ? 1 : -1;
      activeSuggestionIndex =
        (activeSuggestionIndex + delta + optionCount) % optionCount;
      return;
    }

    if (event.key === 'Enter') {
      event.preventDefault();
      if (hasCreateOption && activeSuggestionIndex === 0) {
        createMove();
        return;
      }

      const suggestionIndex = activeSuggestionIndex - suggestionIndexOffset;
      if (suggestionResults.length) {
        selectMove(suggestionResults[suggestionIndex]?.id ?? suggestionResults[0].id);
        return;
      }

      const normalized = normalizeMoveId(query);
      if (normalized && findMove(normalized)) {
        selectMove(normalized);
      }
      return;
    }

    if (event.key === 'Backspace' && !query && hasVisibleSelection) {
      removeMove(selectedIds[selectedIds.length - 1]);
      return;
    }

    if (event.key === 'Escape' && query) {
      event.preventDefault();
      setQuery('');
    }
  }
</script>

<svelte:window on:resize={updateFloatingDropdown} on:scroll={updateFloatingDropdown} />

<div class="move-picker" class:move-picker-with-posters={showPoster}>
  {#if selectedPlacement === 'before' && showSelected && selectedIds.length}
    <div class="move-chip-row draft-move-chip-row">
      {#each selectedIds as moveId}
        <button
          type="button"
          class="move-chip"
          aria-label={`Remove ${moveLabel(moveId)}`}
          on:mousedown|preventDefault
          on:click={() => removeMove(moveId)}
        >
          {moveLabel(moveId)} x
        </button>
      {/each}
    </div>
  {/if}
  <div
    class="move-picker-input-wrap"
    class:move-picker-input-inline={selectedPlacement === 'inside'}
    bind:this={inputWrapElement}
  >
    {#if selectedPlacement === 'inside' && showSelected && selectedIds.length}
      {#each selectedIds as moveId}
        <button
          type="button"
          class="move-chip move-picker-inline-chip"
          aria-label={`Remove ${moveLabel(moveId)}`}
          on:mousedown|preventDefault
          on:click={() => removeMove(moveId)}
        >
          {moveLabel(moveId)} x
        </button>
      {/each}
    {/if}
    <input
      value={query}
      placeholder={inputPlaceholder}
      aria-label={hasVisibleSelection ? addPlaceholder : ariaLabel}
      {autocapitalize}
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      {disabled}
      on:focus={() => {
        dispatch('focus', {});
        void scheduleFloatingDropdownUpdate();
      }}
      on:input={handleInput}
      on:keydown={handleKeydown}
    />
    {#if hasQuery}
      <div
        class="move-picker-dropdown"
        class:move-picker-dropdown-floating={useFloatingDropdown}
        style={useFloatingDropdown ? floatingDropdownStyle : undefined}
        role="listbox"
        aria-label="Matching moves"
      >
        {#if hasCreateOption}
          <button
            type="button"
            role="option"
            class="move-picker-option move-picker-create-option"
            class:active={activeSuggestionIndex === 0}
            aria-selected={activeSuggestionIndex === 0}
            on:mousedown|preventDefault
            on:click={createMove}
            on:mouseenter={() => (activeSuggestionIndex = 0)}
          >
            <span class="move-picker-option-text">
              <strong>{createText}</strong>
              <span>{normalizedQuery}</span>
            </span>
          </button>
        {/if}
        {#if suggestionResults.length}
          {#each suggestionResults as move, index}
            <button
              type="button"
              role="option"
              class="move-picker-option"
              class:active={index + suggestionIndexOffset === activeSuggestionIndex}
              aria-selected={index + suggestionIndexOffset === activeSuggestionIndex}
              on:mousedown|preventDefault
              on:click={() => selectMove(move.id)}
              on:mouseenter={() => (activeSuggestionIndex = index + suggestionIndexOffset)}
            >
              {#if showPoster}
                <span class="move-picker-option-poster" aria-hidden="true">
                  {#if posterSrc(move)}
                    <img src={posterSrc(move)} alt="" loading="lazy" />
                  {:else}
                    <span>No preview</span>
                  {/if}
                </span>
              {/if}
              <span class="move-picker-option-text">
                <strong>{primaryLabel(move)}</strong>
                {#if secondaryLabel(move)}
                  <span>{secondaryLabel(move)}</span>
                {/if}
              </span>
            </button>
          {/each}
          {#if hasMoreSuggestions}
            <span class="move-picker-more">{moreText}</span>
          {/if}
        {:else if !hasCreateOption}
          <span class="move-picker-empty">{emptyText}</span>
        {/if}
      </div>
    {/if}
  </div>
  {#if selectedPlacement === 'after' && showSelected && selectedIds.length}
    <div class="move-chip-row draft-move-chip-row">
      {#each selectedIds as moveId}
        <button
          type="button"
          class="move-chip"
          aria-label={`Remove ${moveLabel(moveId)}`}
          on:mousedown|preventDefault
          on:click={() => removeMove(moveId)}
        >
          {moveLabel(moveId)} x
        </button>
      {/each}
    </div>
  {/if}
</div>
