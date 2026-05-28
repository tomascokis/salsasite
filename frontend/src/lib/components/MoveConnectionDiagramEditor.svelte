<script lang="ts">
  import { createEventDispatcher, tick } from 'svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import MoveRelationshipDiagram from '$lib/components/MoveRelationshipDiagram.svelte';
  import { buildRelationshipDiagram } from '$lib/relationship-diagram';
  import type { MoveRecord } from '$lib/types';

  type ConnectionKind = 'parent' | 'child' | 'related';

  export let moves: MoveRecord[] = [];
  export let currentMove: MoveRecord;
  export let parentIds: string[] = [];
  export let childIds: string[] = [];
  export let relatedMoveIds: string[] = [];

  const dispatch = createEventDispatcher<{
    add: { kind: ConnectionKind; moveId: string };
    remove: { moveId: string };
  }>();

  let addMenuOpen = false;
  let activeKind: ConnectionKind | null = null;
  let addQuery = '';
  let pickerWrap: HTMLDivElement;
  let addMenuElement: HTMLDivElement;

  const relationshipKinds: Array<{ id: ConnectionKind; label: string }> = [
    { id: 'parent', label: 'Add parent' },
    { id: 'child', label: 'Add child' },
    { id: 'related', label: 'Add related' }
  ];

  $: currentMoveId = currentMove.id || '__DRAFT_MOVE__';
  $: editableMove = {
    ...currentMove,
    id: currentMoveId,
    parentIds,
    childIds,
    relatedMoveIds
  };
  $: diagramMoves = [
    ...moves.filter((move) => move.id !== currentMoveId),
    editableMove
  ];
  $: diagram = buildRelationshipDiagram(diagramMoves, currentMoveId);
  $: pickerMoves = moves.filter((move) => move.id !== currentMoveId);
  $: selectedIds = activeKind === 'parent'
    ? parentIds
    : activeKind === 'child'
      ? childIds
      : activeKind === 'related'
        ? relatedMoveIds
        : [];
  $: removableNodeIds = [...new Set([...parentIds, ...childIds, ...relatedMoveIds])];
  $: activeKindLabel = relationshipKinds.find((kind) => kind.id === activeKind)?.label ?? '';
  $: hasConnections = removableNodeIds.length > 0;

  function closeAddMenu() {
    addMenuOpen = false;
    activeKind = null;
    addQuery = '';
  }

  function toggleAddMenu() {
    if (addMenuOpen) {
      closeAddMenu();
      return;
    }
    addMenuOpen = true;
    activeKind = null;
    addQuery = '';
  }

  function handleWindowPointerDown(event: PointerEvent) {
    if (!addMenuOpen || !addMenuElement) return;
    const target = event.target;
    if (target instanceof Node && addMenuElement.contains(target)) return;
    if (target instanceof Element && target.closest('.move-picker-dropdown-floating')) return;
    closeAddMenu();
  }

  function handleWindowKeydown(event: KeyboardEvent) {
    if (event.key === 'Escape' && addMenuOpen) {
      closeAddMenu();
    }
  }

  async function chooseKind(kind: ConnectionKind) {
    activeKind = kind;
    addMenuOpen = true;
    addQuery = '';
    await tick();
    pickerWrap?.querySelector('input')?.focus();
  }

  function addMove(moveId: string) {
    if (!activeKind) return;
    dispatch('add', { kind: activeKind, moveId });
    closeAddMenu();
  }
</script>

<svelte:window on:pointerdown={handleWindowPointerDown} on:keydown={handleWindowKeydown} />

<div class="connection-diagram-editor">
  {#if hasConnections}
    <MoveRelationshipDiagram
      {diagram}
      {removableNodeIds}
      on:remove={(event) => dispatch('remove', { moveId: event.detail.moveId })}
    >
      <div slot="overlay" class="connection-diagram-toolbar">
        <div class="connection-add-menu" bind:this={addMenuElement}>
          <button type="button" class="connection-add-button" aria-expanded={addMenuOpen} on:click={toggleAddMenu}>
            Add
          </button>
          {#if addMenuOpen}
            <div class="connection-add-dropdown">
              {#if activeKind}
                <div class="connection-add-picker" bind:this={pickerWrap}>
                  <span class="connection-add-picker-title">{activeKindLabel}</span>
                  <MovePicker
                    moves={pickerMoves}
                    selectedIds={selectedIds}
                    query={addQuery}
                    placeholder="Search moves"
                    ariaLabel={`${activeKindLabel}: search moves`}
                    showSelected={false}
                    floatingDropdown={true}
                    on:query={(event) => (addQuery = event.detail.query)}
                    on:select={(event) => addMove(event.detail.moveId)}
                  />
                </div>
              {:else}
                {#each relationshipKinds as kind}
                  <button type="button" class="connection-add-choice" on:click={() => chooseKind(kind.id)}>{kind.label}</button>
                {/each}
              {/if}
            </div>
          {/if}
        </div>
      </div>
    </MoveRelationshipDiagram>
  {:else}
    <div class="connection-empty-state">
      <div class="connection-add-menu" bind:this={addMenuElement}>
        <button type="button" class="connection-add-button" aria-expanded={addMenuOpen} on:click={toggleAddMenu}>
          Add relationship
        </button>
        {#if addMenuOpen}
          <div class="connection-add-dropdown">
            {#if activeKind}
              <div class="connection-add-picker" bind:this={pickerWrap}>
                <span class="connection-add-picker-title">{activeKindLabel}</span>
                <MovePicker
                  moves={pickerMoves}
                  selectedIds={selectedIds}
                  query={addQuery}
                  placeholder="Search moves"
                  ariaLabel={`${activeKindLabel}: search moves`}
                  showSelected={false}
                  on:query={(event) => (addQuery = event.detail.query)}
                  on:select={(event) => addMove(event.detail.moveId)}
                />
              </div>
            {:else}
              {#each relationshipKinds as kind}
                <button type="button" class="connection-add-choice" on:click={() => chooseKind(kind.id)}>{kind.label}</button>
              {/each}
            {/if}
          </div>
        {/if}
      </div>
    </div>
  {/if}
</div>
