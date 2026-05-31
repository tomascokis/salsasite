<script lang="ts">
  import { invalidateAll } from '$app/navigation';
  import EditableList from '$lib/components/EditableList.svelte';
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
  let isDeleting = false;
  let status = '';
  let fullName = '';
  let displayName = '';
  let instagramHandle = '';
  let role: DancerRole = 'unknown';
  let level: DancerLevel = 'unknown';
  let region = '';
  let regionQuery = '';

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
  $: dancerListItems = dancers.map((dancer) => ({
    id: dancer.id,
    title: dancer.displayName,
    meta: `${dancer.moves.length} moves · ${dancer.dances.length} dances`,
    badges: dancer.role !== 'unknown' ? [{ label: roleLabel(dancer.role) }] : [],
    active: selectedId === dancer.id
  }));
  $: regionOptions = Array.from(new Set(dancers.map((dancer) => dancer.region).filter((value): value is string => Boolean(value))))
    .sort((left, right) => left.localeCompare(right))
    .map((value) => ({ id: value, label: value }));
  $: selectedRegionIds = region ? [region] : [];
  $: selectedMoveItems = selectedDancer?.moves.map((move) => ({
    id: move.id,
    title: move.name ?? move.id,
    meta: move.topic,
    badges: [{ label: move.id }]
  })) ?? [];

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
    regionQuery = '';
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
    regionQuery = '';
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

  async function deleteDancer() {
    if (!selectedDancer) return;

    const dancerToDelete = selectedDancer;
    const shouldDelete = window.confirm(`Delete "${dancerToDelete.displayName}"? Associated dances and moves will stay in place.`);
    if (!shouldDelete) return;

    isDeleting = true;
    status = 'Deleting...';

    const currentIndex = dancers.findIndex((dancer) => dancer.id === dancerToDelete.id);
    const fallbackDancer = dancers[currentIndex + 1] ?? dancers[currentIndex - 1] ?? null;
    try {
      const response = await fetch('/api/dancers', {
        method: 'DELETE',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ id: dancerToDelete.id })
      });
      const payload = await response.json();

      if (!response.ok) {
        status = payload.error ?? 'Could not delete dancer.';
        return;
      }

      await invalidateAll();
      selectedId = fallbackDancer?.id ?? null;
      isEditing = false;
      status = 'Deleted.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Could not delete dancer.';
    } finally {
      isDeleting = false;
    }
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

      <EditableList
        items={dancerListItems}
        showEdit={false}
        selectable={true}
        emptyText="No dancers yet."
        on:select={(event) => {
          const dancer = dancers.find((candidate) => candidate.id === event.detail.id);
          if (dancer) selectDancer(dancer);
        }}
      />
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
              <SearchablePicker
                options={regionOptions}
                selectedIds={selectedRegionIds}
                query={regionQuery}
                placeholder="Choose or type region"
                addPlaceholder="Change region"
                ariaLabel="Region"
                selectedPlacement="inside"
                allowCreate={true}
                createLabel="Use region"
                on:query={(event) => (regionQuery = event.detail.query)}
                on:select={(event) => {
                  region = event.detail.option.label;
                  regionQuery = '';
                }}
                on:create={(event) => {
                  region = event.detail.value;
                  regionQuery = '';
                }}
                on:remove={() => {
                  region = '';
                  regionQuery = '';
                }}
              />
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
            {#if selectedId}
              <button class="header-button danger-button" type="button" disabled={isDeleting} on:click={deleteDancer}>Delete</button>
            {/if}
            <button type="button" on:click={() => (isEditing = false)}>Cancel</button>
            <button class="header-button" type="button" disabled={isSaving} on:click={saveDancer}>Save</button>
          </div>
        </section>
      {:else if selectedDancer}
        <section class="panel meta-card dancer-profile-card">
          <div class="panel-header">
            <div class="dancer-profile-heading">
              <div class="media-properties-title">
                <h3>{selectedDancer.displayName}</h3>
              </div>
              <div class="dancer-profile-actions">
                <button class="pill" type="button" disabled={isDeleting} on:click={startEditDancer}>Edit</button>
                <button class="pill danger-pill" type="button" disabled={isDeleting} on:click={deleteDancer}>Delete</button>
              </div>
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
            <EditableList items={selectedMoveItems} showEdit={true} emptyText="No moves are associated with this dancer yet.">
              <a slot="edit" let:item href={`/moves/${selectedDancer.moves.find((move) => move.id === item.id)?.slug}`}>Open</a>
            </EditableList>
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
