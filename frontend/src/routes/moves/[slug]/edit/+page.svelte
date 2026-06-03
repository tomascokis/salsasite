<script lang="ts">
  import { goto, invalidateAll } from '$app/navigation';
  import AutoResizeTextarea from '$lib/components/AutoResizeTextarea.svelte';
  import EntityPicker from '$lib/components/EntityPicker.svelte';
  import MoveConnectionDiagramEditor from '$lib/components/MoveConnectionDiagramEditor.svelte';
  import { moveDisplayId } from '$lib/move-id';
  import MoveTypeControl from '$lib/components/MoveTypeControl.svelte';
  import MoveVideoPreview from '$lib/components/MoveVideoPreview.svelte';
  import type { EntityPickerTemplate } from '$lib/components/entity-picker';
  import type { MetadataEntry, MoveRecord, SiteMetadata } from '$lib/types';

  export let data: {
    move: MoveRecord;
    metadata: SiteMetadata;
    moves: MoveRecord[];
    preview: { filePath: string; posterFile: string | null; label: string } | null;
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
  let topicQuery = '';
  let familyQuery = '';
  let positionsQuery = '';
  let tagsQuery = '';
  let sourceQuery = '';
  let status = '';
  let isSaving = false;
  let isReviewOpen = reviewFlag;

  const levelOptions = ['', '1', '2', '3', '4', '5'];
  const topicPickerTemplate: EntityPickerTemplate = {
    key: 'move-topic',
    kind: 'searchable',
    showHeader: false,
    placeholder: 'Search topics',
    addPlaceholder: '',
    ariaLabel: 'Search topics',
    mode: 'singleEdit',
    createPolicy: 'persistMetadata',
    valueSource: 'label',
    createLabel: 'Use topic'
  };
  const familyPickerTemplate: EntityPickerTemplate = {
    key: 'move-family',
    kind: 'searchable',
    showHeader: false,
    placeholder: 'Search families',
    addPlaceholder: '',
    ariaLabel: 'Search families',
    mode: 'singleEdit',
    createPolicy: 'persistMetadata',
    valueSource: 'label',
    createLabel: 'Use family'
  };
  const positionPickerTemplate: EntityPickerTemplate = {
    key: 'move-positions',
    kind: 'searchable',
    showHeader: false,
    placeholder: '',
    addPlaceholder: '',
    ariaLabel: 'Positions',
    mode: 'singleEdit',
    createPolicy: 'local',
    valueSource: 'label',
    createLabel: 'Use position'
  };
  const tagPickerTemplate: EntityPickerTemplate = {
    key: 'move-tags',
    kind: 'searchable',
    showHeader: false,
    placeholder: 'Add tags',
    addPlaceholder: 'Add tags',
    ariaLabel: 'Tags',
    mode: 'multiEdit',
    createPolicy: 'local',
    valueSource: 'label',
    createLabel: 'Add tag'
  };
  const authorshipPickerTemplate: EntityPickerTemplate = {
    key: 'move-authorship',
    kind: 'searchable',
    showHeader: false,
    placeholder: '',
    addPlaceholder: '',
    ariaLabel: 'Authorship',
    mode: 'singleEdit',
    createPolicy: 'local',
    valueSource: 'label',
    createLabel: 'Use authorship'
  };
  $: topicOptions = metadataPickerOptions(data.metadata.topics, topic, 'topic');
  $: familyOptions = metadataPickerOptions(data.metadata.families, group, 'family');
  $: positionOptions = uniqueMoveTextOptions(data.moves.map((move) => move.positions));
  $: sourceOptions = uniqueMoveTextOptions(data.moves.map((move) => move.source));
  $: tagOptions = uniqueMoveTextOptions(data.moves.flatMap((move) => splitTagText(move.tags ?? '')));
  $: selectedTopicIds = metadataSelectionIds(data.metadata.topics, topic, 'topic');
  $: selectedFamilyIds = metadataSelectionIds(data.metadata.families, group, 'family');
  $: selectedPositionIds = positions ? [positions] : [];
  $: selectedSourceIds = source ? [source] : [];
  $: selectedTagIds = splitTagText(tags);
  $: connectionMove = {
    ...data.move,
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
  };

  function metadataOption(entry: MetadataEntry) {
    return {
      id: entry.id,
      label: entry.name,
      secondary: `${entry.moveCount} moves`
    };
  }

  function metadataCustomId(kind: 'topic' | 'family', value: string) {
    return `custom-${kind}:${value.trim().toLocaleLowerCase()}`;
  }

  function metadataSelectionIds(entries: MetadataEntry[], value: string, kind: 'topic' | 'family') {
    const normalized = value.trim().toLocaleLowerCase();
    if (!normalized) return [];
    const existing = entries.find((entry) => entry.name.trim().toLocaleLowerCase() === normalized);
    return [existing?.id ?? metadataCustomId(kind, value)];
  }

  function matchingMetadataIds(entries: MetadataEntry[], value: string) {
    const normalized = value.trim().toLocaleLowerCase();
    return entries.filter((entry) => entry.name.trim().toLocaleLowerCase() === normalized).map((entry) => entry.id);
  }

  function metadataPickerOptions(entries: MetadataEntry[], value: string, kind: 'topic' | 'family') {
    const options = entries.map(metadataOption);
    const customValue = value.trim();
    if (!customValue || matchingMetadataIds(entries, customValue).length) {
      return options;
    }
    return [
      {
        id: metadataCustomId(kind, customValue),
        label: customValue,
        secondary: 'New'
      },
      ...options
    ];
  }

  function uniqueMoveTextOptions(values: Array<string | null | undefined>) {
    return Array.from(new Set(values.map((value) => String(value ?? '').trim()).filter(Boolean)))
      .sort((left, right) => left.localeCompare(right))
      .map((value) => ({ id: value, label: value }));
  }

  function splitTagText(value: string | null | undefined) {
    return String(value ?? '')
      .split(/[,;]+/)
      .map((entry) => entry.trim())
      .filter(Boolean);
  }

  function addTagValue(value: string) {
    const next = value.trim();
    if (!next) return;
    const existing = splitTagText(tags);
    if (!existing.some((entry) => entry.toLocaleLowerCase() === next.toLocaleLowerCase())) {
      tags = [...existing, next].join(', ');
    }
    tagsQuery = '';
  }

  function removeTagValue(value: string) {
    tags = splitTagText(tags)
      .filter((entry) => entry !== value)
      .join(', ');
  }

  async function persistMetadata(kind: 'topic' | 'family', value: string) {
    const name = value.trim();
    if (!name) return false;
    const response = await fetch('/api/metadata', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ kind, name })
    });
    if (!response.ok) {
      const payload = await response.json().catch(() => ({}));
      status = payload.error ?? `Could not save ${kind}.`;
      return false;
    }
    await invalidateAll();
    return true;
  }

  function normalizeMoveId(value: string) {
    return value.trim().toUpperCase();
  }

  function addConnection(kind: 'parent' | 'child' | 'related', selectedMoveId: string) {
    const moveId = normalizeMoveId(selectedMoveId);
    if (!moveId || moveId === data.move.id || !data.moves.some((move) => move.id === moveId)) {
      return;
    }

    if (kind === 'parent' && !parentIds.includes(moveId)) {
      parentIds = [...parentIds, moveId].sort();
    }
    if (kind === 'child' && !childIds.includes(moveId)) {
      childIds = [...childIds, moveId].sort();
    }
    if (kind === 'related' && !relatedMoveIds.includes(moveId)) {
      relatedMoveIds = [...relatedMoveIds, moveId].sort();
    }
  }

  function removeConnection(kind: 'parent' | 'child' | 'related', moveId: string) {
    if (kind === 'parent') parentIds = parentIds.filter((entry) => entry !== moveId);
    if (kind === 'child') childIds = childIds.filter((entry) => entry !== moveId);
    if (kind === 'related') relatedMoveIds = relatedMoveIds.filter((entry) => entry !== moveId);
  }

  function removeConnectionByMoveId(moveId: string) {
    parentIds = parentIds.filter((entry) => entry !== moveId);
    childIds = childIds.filter((entry) => entry !== moveId);
    relatedMoveIds = relatedMoveIds.filter((entry) => entry !== moveId);
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
  <title>Edit {data.move.name ?? moveDisplayId(data.move)} | Salsa Encyclopedia</title>
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
        <span>{moveDisplayId(data.move)}</span>
      </div>
    </div>

    {#if data.preview}
      <div class="move-editor-section move-editor-preview-section">
        <MoveVideoPreview
          filePath={data.preview.filePath}
          posterFile={data.preview.posterFile}
          label={`${data.preview.label} video preview`}
        />
      </div>
    {/if}

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
        <EntityPicker
          template={topicPickerTemplate}
          options={topicOptions}
          selectedIds={selectedTopicIds}
          query={topicQuery}
          onquery={(detail) => {
            topicQuery = detail.query;
          }}
          onselect={(detail) => {
            topic = detail.value;
            topicQuery = '';
          }}
          oncreate={(detail) => {
            topic = detail.value;
            topicQuery = '';
            void persistMetadata('topic', detail.value);
          }}
          onremove={() => {
            topic = '';
            topicQuery = '';
          }}
        />
      </label>
      <label>
        <span>Type</span>
        <MoveTypeControl bind:value={type} />
      </label>
      <label>
        <span>Family</span>
        <EntityPicker
          template={familyPickerTemplate}
          options={familyOptions}
          selectedIds={selectedFamilyIds}
          query={familyQuery}
          onquery={(detail) => {
            familyQuery = detail.query;
          }}
          onselect={(detail) => {
            group = detail.value;
            familyQuery = '';
          }}
          oncreate={(detail) => {
            group = detail.value;
            familyQuery = '';
            void persistMetadata('family', detail.value);
          }}
          onremove={() => {
            group = '';
            familyQuery = '';
          }}
        />
      </label>
      <label>
        <span>Positions</span>
        <EntityPicker
          template={positionPickerTemplate}
          options={positionOptions}
          selectedIds={selectedPositionIds}
          query={positionsQuery}
          onquery={(detail) => (positionsQuery = detail.query)}
          onselect={(detail) => {
            positions = detail.value;
            positionsQuery = '';
          }}
          oncreate={(detail) => {
            positions = detail.value;
            positionsQuery = '';
          }}
          onremove={() => {
            positions = '';
            positionsQuery = '';
          }}
        />
      </label>
      <label>
        <span>Tags</span>
        <EntityPicker
          template={tagPickerTemplate}
          options={tagOptions}
          selectedIds={selectedTagIds}
          query={tagsQuery}
          onquery={(detail) => (tagsQuery = detail.query)}
          onselect={(detail) => addTagValue(detail.value)}
          oncreate={(detail) => addTagValue(detail.value)}
          onremove={(detail) => removeTagValue(detail.value)}
        />
      </label>
      <label>
        <span>Authorship</span>
        <EntityPicker
          template={authorshipPickerTemplate}
          options={sourceOptions}
          selectedIds={selectedSourceIds}
          query={sourceQuery}
          onquery={(detail) => (sourceQuery = detail.query)}
          onselect={(detail) => {
            source = detail.value;
            sourceQuery = '';
          }}
          oncreate={(detail) => {
            source = detail.value;
            sourceQuery = '';
          }}
          onremove={() => {
            source = '';
            sourceQuery = '';
          }}
        />
      </label>
      <label class="wide">
        <span>Description</span>
        <AutoResizeTextarea bind:value={description} />
      </label>
      <label class="wide">
        <span>Comments</span>
        <AutoResizeTextarea bind:value={comments} />
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

    <MoveConnectionDiagramEditor
      moves={data.moves}
      currentMove={connectionMove}
      {parentIds}
      {childIds}
      {relatedMoveIds}
      on:add={(event) => addConnection(event.detail.kind, event.detail.moveId)}
      on:remove={(event) => removeConnectionByMoveId(event.detail.moveId)}
    />
  </section>
</div>
