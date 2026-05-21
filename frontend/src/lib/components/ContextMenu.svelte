<script lang="ts">
  import { createEventDispatcher, onDestroy } from 'svelte';

  type ContextMenuItem = {
    id: string;
    label: string;
    disabled?: boolean;
  };

  export let items: ContextMenuItem[] = [];
  export let disabled = false;
  export let longPressMs = 520;

  const dispatch = createEventDispatcher<{
    select: { id: string; item: ContextMenuItem };
    open: { x: number; y: number };
    close: Record<string, never>;
  }>();

  let open = false;
  let x = 0;
  let y = 0;
  let longPressTimer: ReturnType<typeof setTimeout> | null = null;

  function closeMenu() {
    if (!open) return;
    open = false;
    dispatch('close', {});
  }

  function openMenu(clientX: number, clientY: number) {
    if (disabled || !items.length) return;
    x = clientX;
    y = clientY;
    open = true;
    dispatch('open', { x, y });
  }

  function handleContextMenu(event: MouseEvent) {
    event.preventDefault();
    openMenu(event.clientX, event.clientY);
  }

  function clearLongPressTimer() {
    if (longPressTimer) {
      clearTimeout(longPressTimer);
      longPressTimer = null;
    }
  }

  function handlePointerDown(event: PointerEvent) {
    clearLongPressTimer();
    if (event.pointerType !== 'touch' && event.pointerType !== 'pen') {
      return;
    }

    longPressTimer = setTimeout(() => {
      openMenu(event.clientX, event.clientY);
      longPressTimer = null;
    }, longPressMs);
  }

  function selectItem(item: ContextMenuItem) {
    if (item.disabled) return;
    dispatch('select', { id: item.id, item });
    closeMenu();
  }

  onDestroy(clearLongPressTimer);
</script>

<svelte:window
  on:click={closeMenu}
  on:keydown={(event) => {
    if (event.key === 'Escape') closeMenu();
  }}
  on:pointerup={clearLongPressTimer}
  on:pointercancel={clearLongPressTimer}
/>

<div
  class="context-menu-target"
  role="group"
  on:contextmenu={handleContextMenu}
  on:pointerdown={handlePointerDown}
  on:pointermove={clearLongPressTimer}
>
  <slot />
</div>

{#if open}
  <div
    class="shared-context-menu"
    style={`left: ${x}px; top: ${y}px`}
    role="menu"
    tabindex="-1"
    on:click|stopPropagation
    on:keydown|stopPropagation
  >
    {#each items as item}
      <button type="button" role="menuitem" disabled={item.disabled} on:click={() => selectItem(item)}>
        {item.label}
      </button>
    {/each}
  </div>
{/if}
