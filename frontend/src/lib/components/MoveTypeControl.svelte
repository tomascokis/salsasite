<script lang="ts">
  export let value = '';

  const typeOptions = ['', 'Addition', 'Variation'];

  function optionLabel(option: string) {
    return option || 'Base';
  }
</script>

<span class="segmented-control move-segmented-control move-type-segmented">
  {#each typeOptions as option}
    <button type="button" class:active={value === option} on:click={() => (value = option)}>
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
