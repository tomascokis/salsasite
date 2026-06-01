<script lang="ts">
  import { onMount, tick } from 'svelte';

  export let value = '';

  const typeOptions = ['', 'Addition', 'Variation'];
  const overflowTolerance = 1;

  let compact = false;
  let rootElement: HTMLDivElement;
  let measurementSegmented: HTMLSpanElement;
  let resizeObserver: ResizeObserver | null = null;
  let animationFrame = 0;

  function optionLabel(option: string) {
    return option || 'Base';
  }

  function updateCompactMode() {
    if (!rootElement || !measurementSegmented) return;

    const rootWidth = rootElement.getBoundingClientRect().width;
    if (rootWidth <= 0) return;

    const buttons = Array.from(measurementSegmented.querySelectorAll('button'));
    const segmentedOverflows = measurementSegmented.scrollWidth - measurementSegmented.clientWidth > overflowTolerance;
    const buttonOverflows = buttons.some((button) => button.scrollWidth - button.clientWidth > overflowTolerance);
    const nextCompact = segmentedOverflows || buttonOverflows;

    if (compact !== nextCompact) {
      compact = nextCompact;
    }
  }

  function scheduleFitCheck() {
    if (animationFrame) {
      cancelAnimationFrame(animationFrame);
    }

    animationFrame = requestAnimationFrame(() => {
      animationFrame = 0;
      updateCompactMode();
    });
  }

  onMount(() => {
    void tick().then(scheduleFitCheck);

    resizeObserver = new ResizeObserver(scheduleFitCheck);
    resizeObserver.observe(rootElement);
    window.addEventListener('resize', scheduleFitCheck);
    void document.fonts?.ready.then(scheduleFitCheck);

    return () => {
      if (animationFrame) {
        cancelAnimationFrame(animationFrame);
      }
      resizeObserver?.disconnect();
      window.removeEventListener('resize', scheduleFitCheck);
    };
  });
</script>

<div class="move-type-control" class:move-type-control-compact={compact} bind:this={rootElement}>
  <span class="segmented-control move-segmented-control move-type-segmented" aria-hidden={compact}>
    {#each typeOptions as option}
      <button type="button" class:active={value === option} on:click={() => (value = option)}>
        {optionLabel(option)}
      </button>
    {/each}
  </span>

  <span
    class="segmented-control move-segmented-control move-type-segmented move-type-segmented-measure"
    aria-hidden="true"
    bind:this={measurementSegmented}
  >
    {#each typeOptions as option}
      <button type="button" tabindex="-1" class:active={value === option}>
        {optionLabel(option)}
      </button>
    {/each}
  </span>

  <div class="move-type-select-wrap">
    <select class="move-type-select" aria-label="Type" bind:value>
      {#each typeOptions as option}
        <option value={option}>{optionLabel(option)}</option>
      {/each}
    </select>
  </div>
</div>
