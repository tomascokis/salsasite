<script lang="ts">
  import { invalidateAll } from '$app/navigation';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import type { DancerLevel, DancerProfile, DancerRole } from '$lib/types';

  type PickerOption = {
    id: string;
    label: string;
    secondary?: string | null;
  };

  export let data: {
    dancers: DancerProfile[];
  };

  const roleOptions: Array<{ value: DancerRole; label: string }> = [
    { value: 'unknown', label: 'Unknown' },
    { value: 'lead', label: 'Lead' },
    { value: 'follow', label: 'Follow' }
  ];
  const levelOptions: Array<{ value: DancerLevel; label: string }> = [
    { value: 'unknown', label: 'Unknown' },
    { value: 'world-class', label: 'World class' },
    { value: 'pro', label: 'Pro' },
    { value: 'semi-pro', label: 'Semi-pro' },
    { value: 'amateur', label: 'Amateur' }
  ];

  let dancers = data.dancers;
  let selectedId: string | null = dancers[0]?.id ?? null;
  let query = '';
  let isEditing = false;
  let isSaving = false;
  let status = '';
  let fullName = '';
  let displayName = '';
  let instagramHandle = '';
  let role: DancerRole = 'unknown';
  let level: DancerLevel = 'unknown';
  let region = '';

  $: selectedDancer = dancers.find((dancer) => dancer.id === selectedId) ?? null;
  $: pickerOptions = dancers.map(
    (dancer): PickerOption => ({
      id: dancer.id,
      label: dancer.displayName,
      secondary: [dancer.fullName !== dancer.displayName ? dancer.fullName : null, dancer.region, `${dancer.moves.length} moves`]
        .filter(Boolean)
        .join(' · ')
    })
  );

  $: if (data.dancers !== dancers) {
    dancers = data.dancers;
  }

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }

  function roleLabel(value: DancerRole) {
    return roleOptions.find((option) => option.value === value)?.label ?? value;
  }

  function levelLabel(value: DancerLevel) {
    return levelOptions.find((option) => option.value === value)?.label ?? value;
  }

  function selectDancer(dancer: DancerProfile) {
    selectedId = dancer.id;
    isEditing = false;
    status = '';
  }

  function startNewDancer() {
    selectedId = null;
    fullName = '';
    displayName = '';
    instagramHandle = '';
    role = 'unknown';
    level = 'unknown';
    region = '';
    isEditing = true;
    status = '';
  }

  function startEditDancer() {
    if (!selectedDancer) return;
    fullName = selectedDancer.fullName;
    displayName = selectedDancer.displayName;
    instagramHandle = selectedDancer.instagramHandle ?? '';
    role = selectedDancer.role;
    level = selectedDancer.level;
    region = selectedDancer.region ?? '';
    isEditing = true;
    status = '';
  }

  async function saveDancer() {
    isSaving = true;
    status = 'Saving...';

    const response = await fetch('/api/dancers', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        id: selectedId,
        fullName,
        displayName,
        instagramHandle,
        role,
        level,
        region
      })
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not save dancer.';
      return;
    }

    await invalidateAll();
    selectedId = payload.dancer.id;
    isEditing = false;
    status = 'Saved.';
  }
</script>

<svelte:head>
  <title>Dancers | Salsa Encyclopedia</title>
</svelte:head>

<div class="dancers-page">
  <div class="move-editor-topbar">
    <button class="header-button" type="button" on:click={startNewDancer}>New dancer</button>
    {#if status}
      <span class="muted">{status}</span>
    {/if}
  </div>

  <div class="dancers-layout">
    <aside class="panel meta-card dancers-list-panel">
      <div class="panel-header">
        <div class="media-properties-title">
          <h3>Dancers</h3>
          <span>{dancers.length} profiles</span>
        </div>
      </div>

      <SearchablePicker
        options={pickerOptions}
        selectedIds={selectedId ? [selectedId] : []}
        {query}
        placeholder="Search dancers"
        addPlaceholder="Search dancers"
        ariaLabel="Search dancers"
        showSelected={false}
        on:query={(event) => (query = event.detail.query)}
        on:select={(event) => {
          const dancer = dancers.find((candidate) => candidate.id === event.detail.id);
          if (dancer) selectDancer(dancer);
        }}
      />

      <div class="editable-list dancer-entry-list">
        {#each dancers as dancer}
          <button type="button" class="editable-list-row dancer-entry-row" class:active={selectedId === dancer.id} on:click={() => selectDancer(dancer)}>
            <span class="editable-list-main">
              <span class="editable-list-title-row">
                <strong>{dancer.displayName}</strong>
                {#if dancer.role !== 'unknown'}
                  <ContentBadge label={roleLabel(dancer.role)} tone="info" />
                {/if}
              </span>
              <span class="editable-list-meta">
                {dancer.moves.length} moves · {dancer.dances.length} dances
              </span>
            </span>
          </button>
        {/each}
      </div>
    </aside>

    <main class="dancer-profile">
      {#if isEditing}
        <section class="panel meta-card dancer-profile-card">
          <div class="panel-header">
            <h3>{selectedId ? 'Edit dancer' : 'New dancer'}</h3>
          </div>
          <div class="move-editor-grid metadata-editor-grid">
            <label>
              <span>Full name</span>
              <input bind:value={fullName} />
            </label>
            <label>
              <span>Display name</span>
              <input bind:value={displayName} />
            </label>
            <label>
              <span>Instagram</span>
              <input bind:value={instagramHandle} placeholder="@handle" />
            </label>
            <label>
              <span>Region</span>
              <input bind:value={region} />
            </label>
            <label>
              <span>Role</span>
              <select bind:value={role}>
                {#each roleOptions as option}
                  <option value={option.value}>{option.label}</option>
                {/each}
              </select>
            </label>
            <label>
              <span>Level</span>
              <select bind:value={level}>
                {#each levelOptions as option}
                  <option value={option.value}>{option.label}</option>
                {/each}
              </select>
            </label>
          </div>
          <div class="move-editor-actions metadata-actions">
            <button type="button" on:click={() => (isEditing = false)}>Cancel</button>
            <button class="header-button" type="button" disabled={isSaving} on:click={saveDancer}>Save</button>
          </div>
        </section>
      {:else if selectedDancer}
        <section class="panel meta-card dancer-profile-card">
          <div class="panel-header">
            <div class="media-properties-title">
              <h3>{selectedDancer.displayName}</h3>
              <button class="pill" type="button" on:click={startEditDancer}>Edit</button>
            </div>
          </div>

          <div class="dancer-summary">
            <div>
              <strong>Full name</strong>
              <span>{selectedDancer.fullName}</span>
            </div>
            <div>
              <strong>Instagram</strong>
              <span>{selectedDancer.instagramHandle ? `@${selectedDancer.instagramHandle}` : 'Not recorded'}</span>
            </div>
            <div>
              <strong>Role</strong>
              <span>{roleLabel(selectedDancer.role)}</span>
            </div>
            <div>
              <strong>Level</strong>
              <span>{levelLabel(selectedDancer.level)}</span>
            </div>
            <div>
              <strong>Region</strong>
              <span>{selectedDancer.region ?? 'Not recorded'}</span>
            </div>
          </div>
        </section>

        <section class="panel meta-card dancer-profile-card">
          <div class="panel-header">
            <h3>Dances</h3>
          </div>
          {#if selectedDancer.dances.length}
            <div class="media-card-grid dancer-dance-grid">
              {#each selectedDancer.dances as dance}
                <a class="media-gallery-card" href={dance.href}>
                  <span class="media-gallery-poster">
                    {#if dance.posterFile}
                      <img src={posterUrl(dance.posterFile)} alt="" loading="lazy" />
                    {:else}
                      <span>No preview</span>
                    {/if}
                  </span>
                  <span class="media-gallery-card-body">
                    <strong>{dance.displayName}</strong>
                    <span>{dance.meta}</span>
                  </span>
                </a>
              {/each}
            </div>
          {:else}
            <p class="muted">No dances are associated with this dancer yet.</p>
          {/if}
        </section>

        <section class="panel meta-card dancer-profile-card">
          <div class="panel-header">
            <h3>Moves</h3>
          </div>
          {#if selectedDancer.moves.length}
            <div class="editable-list">
              {#each selectedDancer.moves as move}
                <a class="editable-list-row dancer-move-row" href={`/moves/${move.slug}`}>
                  <span class="editable-list-main">
                    <span class="editable-list-title-row">
                      <strong>{move.name ?? move.id}</strong>
                      <ContentBadge label={move.id} />
                    </span>
                    {#if move.topic}
                      <span class="editable-list-meta">{move.topic}</span>
                    {/if}
                  </span>
                </a>
              {/each}
            </div>
          {:else}
            <p class="muted">No moves are associated with this dancer yet.</p>
          {/if}
        </section>
      {:else}
        <section class="panel meta-card dancer-profile-card">
          <p class="muted">Select a dancer or create a new profile.</p>
        </section>
      {/if}
    </main>
  </div>
</div>
