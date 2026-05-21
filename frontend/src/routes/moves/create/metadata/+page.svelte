<script lang="ts">
  import { invalidateAll } from '$app/navigation';
  import ContentBadge from '$lib/components/ContentBadge.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import type { MetadataEntry, MetadataKind, SiteMetadata } from '$lib/types';

  type SearchablePickerOption = {
    id: string;
    label: string;
    secondary?: string | null;
  };

  export let data: {
    metadata: SiteMetadata;
  };

  let metadata = data.metadata;
  let selectedKind: MetadataKind = 'topic';
  let selectedId: string | null = metadata.topics[0]?.id ?? null;
  let name = '';
  let description = '';
  let status = '';
  let isSaving = false;
  let pickerQuery = '';

  $: entries = selectedKind === 'topic' ? metadata.topics : metadata.families;
  $: selectedEntry = entries.find((entry) => entry.id === selectedId) ?? null;
  $: pickerOptions = entries.map(
    (entry): SearchablePickerOption => ({
      id: entry.id,
      label: entry.name,
      secondary: `${entry.moveCount} moves`
    })
  );

  $: if (selectedEntry) {
    name = selectedEntry.name;
    description = selectedEntry.description ?? '';
  }

  function entryUrl(kind: MetadataKind, entry: MetadataEntry) {
    return kind === 'topic' ? `/topics/${entry.slug}` : `/families/${entry.slug}`;
  }

  function selectKind(kind: MetadataKind) {
    selectedKind = kind;
    const nextEntries = kind === 'topic' ? metadata.topics : metadata.families;
    selectedId = nextEntries[0]?.id ?? null;
    pickerQuery = '';
    status = '';
  }

  function selectEntry(entry: MetadataEntry) {
    selectedId = entry.id;
    name = entry.name;
    description = entry.description ?? '';
    status = '';
  }

  function startNewEntry() {
    selectedId = null;
    name = '';
    description = '';
    status = '';
  }

  async function saveEntry() {
    isSaving = true;
    status = 'Saving...';

    const response = await fetch('/api/metadata', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        kind: selectedKind,
        id: selectedId,
        name,
        description
      })
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not save metadata.';
      return;
    }

    await invalidateAll();
    metadata = data.metadata;
    selectedId = payload.entry.id;
    status = 'Saved.';
  }
</script>

<svelte:head>
  <title>Create metadata | Salsa Encyclopedia</title>
</svelte:head>

<div class="metadata-page">
  <div class="move-editor-topbar">
    <a class="pill move-backlink" href="/moves/create">Back to create</a>
    <div class="segmented-control metadata-kind-toggle" aria-label="Metadata kind">
      <button type="button" class:active={selectedKind === 'topic'} on:click={() => selectKind('topic')}>Topics</button>
      <button type="button" class:active={selectedKind === 'family'} on:click={() => selectKind('family')}>Families</button>
    </div>
    <button class="header-button" type="button" on:click={startNewEntry}>New {selectedKind}</button>
  </div>

  <div class="metadata-layout">
    <aside class="panel meta-card metadata-list-panel">
      <div class="panel-header">
        <div class="media-properties-title">
          <h3>{selectedKind === 'topic' ? 'Topics' : 'Families'}</h3>
          <span>{entries.length} entries</span>
        </div>
      </div>

      <SearchablePicker
        options={pickerOptions}
        selectedIds={selectedId ? [selectedId] : []}
        query={pickerQuery}
        placeholder={`Search ${selectedKind === 'topic' ? 'topics' : 'families'}`}
        addPlaceholder={`Search ${selectedKind === 'topic' ? 'topics' : 'families'}`}
        ariaLabel={`Search ${selectedKind === 'topic' ? 'topics' : 'families'}`}
        showSelected={false}
        on:query={(event) => (pickerQuery = event.detail.query)}
        on:select={(event) => {
          const entry = entries.find((candidate) => candidate.id === event.detail.id);
          if (entry) selectEntry(entry);
        }}
      />

      <div class="editable-list metadata-entry-list">
        {#each entries as entry}
          <button type="button" class="editable-list-row metadata-entry-row" class:active={selectedId === entry.id} on:click={() => selectEntry(entry)}>
            <span class="editable-list-main">
              <span class="editable-list-title-row">
                <strong>{entry.name}</strong>
                <ContentBadge label={`${entry.moveCount} moves`} />
                {#if entry.source === 'custom'}
                  <ContentBadge label="Custom" tone="info" />
                {/if}
              </span>
              {#if entry.description}
                <span class="editable-list-meta">{entry.description}</span>
              {/if}
            </span>
          </button>
        {/each}
      </div>
    </aside>

    <main class="panel meta-card metadata-editor-panel">
      <div class="panel-header">
        <div class="media-properties-title">
          <h3>{selectedId ? 'Edit metadata' : `New ${selectedKind}`}</h3>
          {#if selectedEntry}
            <a class="pill" href={entryUrl(selectedKind, selectedEntry)}>View splash page</a>
          {/if}
        </div>
      </div>

      <div class="move-editor-grid metadata-editor-grid">
        <label class="wide">
          <span>Name</span>
          <input bind:value={name} placeholder={selectedKind === 'topic' ? 'Topic name' : 'Family name'} />
        </label>
        <label class="wide">
          <span>Description</span>
          <textarea bind:value={description}></textarea>
        </label>
      </div>

      <div class="move-editor-actions metadata-actions">
        {#if status}
          <span class="muted">{status}</span>
        {/if}
        <button class="header-button" type="button" disabled={isSaving} on:click={saveEntry}>Save</button>
      </div>
    </main>
  </div>
</div>
