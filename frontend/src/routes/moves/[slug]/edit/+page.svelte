<script lang="ts">
  import { goto } from '$app/navigation';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
  import type { MetadataEntry, MoveRecord, SiteMetadata } from '$lib/types';

  type MoveOption = {
    id: string;
    slug: string;
    name: string | null;
  };

  export let data: {
    move: MoveRecord;
    metadata: SiteMetadata;
    moves: MoveOption[];
  };

  let name = data.move.name ?? '';
  let topic = data.move.topic ?? '';
  let level = data.move.level ?? '';
  let type = data.move.type ?? '';
  let group = data.move.group ?? '';
  let positions = data.move.positions ?? '';
  let tags = data.move.tags ?? '';
  let source = data.move.source ?? '';
  let description = data.move.description ?? '';
  let comments = data.move.comments ?? '';
  let reviewFlag = Boolean(data.move.reviewFlag);
  let reviewNotes = data.move.reviewNotes ?? '';
  let parentIds = [...data.move.parentIds];
  let childIds = [...data.move.childIds];
  let relatedMoveIds = [...data.move.relatedMoveIds];
  let parentDraft = '';
  let childDraft = '';
  let relatedDraft = '';
  let topicQuery = '';
  let familyQuery = '';
  let status = '';
  let isSaving = false;
  let isReviewOpen = reviewFlag;

  const options = data.moves.filter((move) => move.id !== data.move.id);
  const typeOptions = ['Addition', 'Variation', ''];
  const levelOptions = ['', '1', '2', '3', '4', '5'];
  $: topicOptions = data.metadata.topics.map(metadataOption);
  $: familyOptions = data.metadata.families.map(metadataOption);
  $: selectedTopicIds = topic ? matchingMetadataIds(data.metadata.topics, topic) : [];
  $: selectedFamilyIds = group ? matchingMetadataIds(data.metadata.families, group) : [];

  function metadataOption(entry: MetadataEntry) {
    return {
      id: entry.id,
      label: entry.name,
      secondary: `${entry.moveCount} moves`
    };
  }

  function matchingMetadataIds(entries: MetadataEntry[], value: string) {
    const normalized = value.trim().toLocaleLowerCase();
    return entries.filter((entry) => entry.name.trim().toLocaleLowerCase() === normalized).map((entry) => entry.id);
  }

  function normalizeMoveId(value: string) {
    return value.trim().toUpperCase();
  }

  function addConnection(kind: 'parent' | 'child' | 'related', selectedMoveId?: string) {
    const draft = selectedMoveId ?? (kind === 'parent' ? parentDraft : kind === 'child' ? childDraft : relatedDraft);
    const moveId = normalizeMoveId(draft);
    if (!moveId || moveId === data.move.id || !data.moves.some((move) => move.id === moveId)) {
      return;
    }

    if (kind === 'parent' && !parentIds.includes(moveId)) {
      parentIds = [...parentIds, moveId].sort();
      parentDraft = '';
    }
    if (kind === 'child' && !childIds.includes(moveId)) {
      childIds = [...childIds, moveId].sort();
      childDraft = '';
    }
    if (kind === 'related' && !relatedMoveIds.includes(moveId)) {
      relatedMoveIds = [...relatedMoveIds, moveId].sort();
      relatedDraft = '';
    }
  }

  function removeConnection(kind: 'parent' | 'child' | 'related', moveId: string) {
    if (kind === 'parent') parentIds = parentIds.filter((entry) => entry !== moveId);
    if (kind === 'child') childIds = childIds.filter((entry) => entry !== moveId);
    if (kind === 'related') relatedMoveIds = relatedMoveIds.filter((entry) => entry !== moveId);
  }

  function updateConnectionQuery(kind: 'parent' | 'child' | 'related', query: string) {
    if (kind === 'parent') parentDraft = query;
    if (kind === 'child') childDraft = query;
    if (kind === 'related') relatedDraft = query;
  }

  async function saveMove() {
    isSaving = true;
    status = 'Saving...';

    const response = await fetch(`/api/moves/${encodeURIComponent(data.move.id)}`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        name,
        topic,
        level,
        type,
        group,
        positions,
        tags,
        source,
        description,
        comments,
        reviewFlag,
        reviewNotes,
        parentIds,
        childIds,
        relatedMoveIds
      })
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not save move.';
      return;
    }

    status = 'Saved.';
    await goto(`/moves/${payload.move.slug}`);
  }
</script>

<svelte:head>
  <title>Edit {data.move.name ?? data.move.id} | Salsa Encyclopedia</title>
</svelte:head>

<div class="move-editor-page">
  <div class="move-editor-topbar">
    <a class="pill move-backlink" href={`/moves/${data.move.slug}`}>Exit</a>
    <button class="header-button" type="button" disabled={isSaving} on:click={saveMove}>Save</button>
    {#if status}
      <span class="muted">{status}</span>
    {/if}
  </div>

  <section class="panel meta-card move-editor-card">
    <div class="panel-header">
      <div class="media-properties-title">
        <h3>Edit move</h3>
        <span>{data.move.id}</span>
      </div>
    </div>

    <div class="move-editor-grid">
      <label>
        <span>Name</span>
        <input bind:value={name} />
      </label>
      <label>
        <span>Level</span>
        <select bind:value={level}>
          {#each levelOptions as option}
            <option value={option}>{option || 'Unspecified'}</option>
          {/each}
        </select>
      </label>
      <label>
        <span>Topic</span>
        <SearchablePicker
          options={topicOptions}
          selectedIds={selectedTopicIds}
          query={topicQuery}
          placeholder="Search topics"
          addPlaceholder="Search topics"
          ariaLabel="Search topics"
          showSelected={false}
          on:query={(event) => {
            topicQuery = event.detail.query;
            topic = event.detail.query;
          }}
          on:select={(event) => {
            topic = event.detail.option.label;
            topicQuery = '';
          }}
        />
      </label>
      <label>
        <span>Type</span>
        <span class="segmented-control move-segmented-control">
          {#each typeOptions as option}
            <button type="button" class:active={type === option} on:click={() => (type = option)}>
              {option || 'Neither'}
            </button>
          {/each}
        </span>
      </label>
      <label>
        <span>Family</span>
        <SearchablePicker
          options={familyOptions}
          selectedIds={selectedFamilyIds}
          query={familyQuery}
          placeholder="Search families"
          addPlaceholder="Search families"
          ariaLabel="Search families"
          showSelected={false}
          on:query={(event) => {
            familyQuery = event.detail.query;
            group = event.detail.query;
          }}
          on:select={(event) => {
            group = event.detail.option.label;
            familyQuery = '';
          }}
        />
      </label>
      <label>
        <span>Positions</span>
        <input bind:value={positions} />
      </label>
      <label>
        <span>Tags</span>
        <input bind:value={tags} />
      </label>
      <label>
        <span>Source</span>
        <input bind:value={source} />
      </label>
      <label class="wide">
        <span>Description</span>
        <textarea bind:value={description}></textarea>
      </label>
      <label class="wide">
        <span>Comments</span>
        <textarea bind:value={comments}></textarea>
      </label>
    </div>
  </section>

  <section class="panel meta-card move-editor-card">
    <button type="button" class="review-toggle" on:click={() => (isReviewOpen = !isReviewOpen)}>
      <span>Review</span>
      <span>{reviewFlag ? 'Flagged' : 'Hidden'}</span>
    </button>

    {#if isReviewOpen}
      <div class="move-editor-grid review-editor-grid">
        <label class="review-checkbox">
          <input type="checkbox" bind:checked={reviewFlag} />
          <span>Flag this move for review</span>
        </label>
        <label class="wide">
          <span>Review notes</span>
          <textarea bind:value={reviewNotes}></textarea>
        </label>
      </div>
    {/if}
  </section>

  <section class="panel meta-card move-editor-card">
    <div class="panel-header">
      <h3>Connections</h3>
    </div>

    <div class="connection-editor-grid">
      <div class="connection-editor">
        <h4>Parents</h4>
        <MovePicker
          moves={options}
          selectedIds={parentIds}
          query={parentDraft}
          on:query={(event) => updateConnectionQuery('parent', event.detail.query)}
          on:select={(event) => addConnection('parent', event.detail.moveId)}
          on:remove={(event) => removeConnection('parent', event.detail.moveId)}
        />
      </div>

      <div class="connection-editor">
        <h4>Children</h4>
        <MovePicker
          moves={options}
          selectedIds={childIds}
          query={childDraft}
          on:query={(event) => updateConnectionQuery('child', event.detail.query)}
          on:select={(event) => addConnection('child', event.detail.moveId)}
          on:remove={(event) => removeConnection('child', event.detail.moveId)}
        />
      </div>

      <div class="connection-editor">
        <h4>Related moves</h4>
        <MovePicker
          moves={options}
          selectedIds={relatedMoveIds}
          query={relatedDraft}
          on:query={(event) => updateConnectionQuery('related', event.detail.query)}
          on:select={(event) => addConnection('related', event.detail.moveId)}
          on:remove={(event) => removeConnection('related', event.detail.moveId)}
        />
      </div>
    </div>
  </section>
</div>
