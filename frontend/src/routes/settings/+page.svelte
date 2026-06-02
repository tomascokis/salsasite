<script lang="ts">
  import { onMount } from 'svelte';
  import {
    applyBadgeColors,
    DEFAULT_BADGE_COLORS,
    loadBadgeColors,
    normalizeBadgeColors,
    saveBadgeColors,
    type BadgeColorKey,
    type BadgeColors
  } from '$lib/badge-settings';

  const badgeOptions: Array<{ key: BadgeColorKey; label: string; sample: string; className: string }> = [
    { key: 'timingOn1', label: 'Timing: On1', sample: 'On1', className: 'badge-timing-on1' },
    { key: 'timingOn2', label: 'Timing: On2', sample: 'On2', className: 'badge-timing-on2' },
    { key: 'timingOther', label: 'Timing: Other', sample: 'Other', className: 'badge-timing-other' },
    { key: 'contentMusic', label: 'Content: Music', sample: 'Music', className: 'badge-content-music' },
    { key: 'contentCounts', label: 'Content: Counts', sample: 'Counts', className: 'badge-content-counts' },
    { key: 'contentOther', label: 'Content: Other', sample: 'Other', className: 'badge-content-other' },
    { key: 'environmentClass', label: 'Environment: Class', sample: 'Class', className: 'badge-environment-class' },
    { key: 'environmentSocial', label: 'Environment: Social', sample: 'Social', className: 'badge-environment-social' }
  ];

  let colors: BadgeColors = { ...DEFAULT_BADGE_COLORS };
  let savedStatus = '';

  onMount(() => {
    colors = loadBadgeColors();
    applyBadgeColors(colors);
  });

  function setColor(key: BadgeColorKey, value: string) {
    colors = normalizeBadgeColors({ ...colors, [key]: value });
    saveBadgeColors(colors);
    applyBadgeColors(colors);
    savedStatus = 'Saved';
  }

  function resetColors() {
    colors = { ...DEFAULT_BADGE_COLORS };
    saveBadgeColors(colors);
    applyBadgeColors(colors);
    savedStatus = 'Reset';
  }
</script>

<svelte:head>
  <title>Settings | Salsa Encyclopedia</title>
</svelte:head>

<div class="settings-page">
  <section class="settings-panel">
    <div class="settings-header">
      <h2>Labs</h2>
      <span class="settings-header-actions">
        <a class="header-button" href="/settings/history">Action history</a>
        <a class="header-button" href="/settings/pickers">Open picker lab</a>
      </span>
    </div>
    <p class="muted settings-status">Shared family, dancer, and move picker test page.</p>
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h2>Display</h2>
      <button type="button" on:click={resetColors}>Reset</button>
    </div>

    <div class="settings-preview" aria-label="Badge colour preview">
      {#each badgeOptions as option}
        <span class={`media-property-badge ${option.className}`}>{option.sample}</span>
      {/each}
    </div>

    <div class="settings-fields">
      {#each badgeOptions as option}
        <label class="settings-color-field">
          <span>{option.label}</span>
          <input
            type="color"
            value={colors[option.key]}
            on:input={(event) => setColor(option.key, (event.currentTarget as HTMLInputElement).value)}
          />
          <input
            type="text"
            value={colors[option.key]}
            inputmode="text"
            on:change={(event) => setColor(option.key, (event.currentTarget as HTMLInputElement).value)}
          />
        </label>
      {/each}
    </div>

    {#if savedStatus}
      <p class="muted settings-status">{savedStatus}</p>
    {/if}
  </section>
</div>
