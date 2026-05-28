<script lang="ts">
  import { tick } from 'svelte';

  export let value = '';
  export let rows = 2;
  export let maxHeight = 180;

  let textarea: HTMLTextAreaElement;

  function resize() {
    if (!textarea) return;
    textarea.style.height = 'auto';
    const nextHeight = Math.min(textarea.scrollHeight, maxHeight);
    textarea.style.height = `${nextHeight}px`;
    textarea.style.overflowY = textarea.scrollHeight > maxHeight ? 'auto' : 'hidden';
  }

  function handleInput() {
    resize();
  }

  $: value, void tick().then(resize);
</script>

<textarea
  class="auto-resize-textarea"
  bind:this={textarea}
  bind:value
  {rows}
  on:input={handleInput}
></textarea>
