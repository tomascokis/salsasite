<script lang="ts">
  import { browser } from '$app/environment';
  import { goto, invalidateAll } from '$app/navigation';
  import { onDestroy, onMount } from 'svelte';
  import AutoResizeTextarea from '$lib/components/AutoResizeTextarea.svelte';
  import ContextMenu from '$lib/components/ContextMenu.svelte';
  import EntityPicker from '$lib/components/EntityPicker.svelte';
  import MoveConnectionDiagramEditor from '$lib/components/MoveConnectionDiagramEditor.svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import MoveTypeControl from '$lib/components/MoveTypeControl.svelte';
  import MoveVideoPreview from '$lib/components/MoveVideoPreview.svelte';
  import type { EntityPickerTemplate } from '$lib/components/entity-picker';
  import { moveDisplayId, normalizeMoveDisplayId } from '$lib/move-id';
  import { draftMoveIdFromName } from '$lib/move-id-utils.js';
  import type { MetadataEntry, MoveRecord, SiteMetadata } from '$lib/types';

  type MoveDraft = {
    draftId: string;
    move: MoveRecord;
    createdAt: string;
    updatedAt: string;
  };

  type MoveCardRecord = MoveRecord & {
    posterFile: string | null;
  };

  type MovePreview = {
    filePath: string;
    posterFile: string | null;
    label: string;
  };

  type ReviewMoveEntry =
    | { kind: 'draft'; draft: MoveDraft; move: MoveRecord }
    | { kind: 'published'; move: MoveCardRecord };

  type SelectedEditor =
    | { kind: 'draft'; id: string }
    | { kind: 'published'; id: string }
    | { kind: 'new'; id: null }
    | null;

  export let data: {
    moves: MoveCardRecord[];
    drafts: MoveDraft[];
    metadata: SiteMetadata;
    draftPreviews: Record<string, MovePreview>;
    recentMoveIds: string[];
  };

  let drafts = data.drafts;
  let selectedEditor: SelectedEditor = null;
  let id = '';
  let name = '';
  let topic = '';
  let level = '';
  let type = '';
  let group = '';
  let positions = '';
  let tags = '';
  let source = '';
  let description = '';
  let comments = '';
  let reviewFlag = false;
  let reviewNotes = '';
  let parentIds: string[] = [];
  let childIds: string[] = [];
  let relatedMoveIds: string[] = [];
  let topicQuery = '';
  let familyQuery = '';
  let positionsQuery = '';
  let tagsQuery = '';
  let sourceQuery = '';
  let status = '';
  let isSaving = false;
  let pinSearch = '';
  let pinnedMoveIds = [...data.recentMoveIds];
  let reviewMoveEntries: ReviewMoveEntry[] = [];
  let isReviewOpen = false;
  let isReviewListOpen = true;
  let autosaveTimer: ReturnType<typeof setTimeout> | null = null;
  let lastAutosaveSignature = '';
  const pinnedStorageKey = 'salsa-encyclopedia:pinned-create-moves';
  const autosaveDelayMs = 900;
  const levelOptions = ['', '1', '2', '3', '4', '5'];
  const relationshipMenuItems = [
    { id: 'parent', label: 'Add as parent' },
    { id: 'child', label: 'Add as child' },
    { id: 'related', label: 'Add as related' }
  ];
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

  $: pinnedMoves = pinnedMoveIds
    .map((moveId) => data.moves.find((move) => move.id === moveId))
    .filter((move): move is MoveCardRecord => Boolean(move));
  $: reviewMoveEntries = [
    ...drafts.filter((draft) => draft.move.reviewFlag).map((draft) => ({ kind: 'draft' as const, draft, move: draft.move })),
    ...data.moves.filter((move) => move.reviewFlag).map((move) => ({ kind: 'published' as const, move }))
  ];
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
  $: currentDisplayId = normalizeMoveDisplayId(id) ?? '';
  $: currentMoveId = selectedEditor?.kind === 'published' ? selectedEditor.id : normalizeMoveId(id);
  $: selectedPublishedMove = selectedEditor?.kind === 'published'
    ? data.moves.find((move) => move.id === selectedEditor.id) ?? null
    : null;
  $: selectedPublishedPreview = previewFromMove(selectedPublishedMove);
  $: selectedDraftPreview =
    selectedEditor?.kind === 'draft'
      ? data.draftPreviews[selectedEditor.id] ?? data.draftPreviews[currentMoveId]
      : null;
  $: selectedMovePreview = selectedDraftPreview ?? selectedPublishedPreview;
  $: connectionMove = {
    id: currentMoveId || '__DRAFT_MOVE__',
    slug: currentMoveId || 'draft-move',
    displayId: currentDisplayId || currentMoveId || null,
    name: name || currentDisplayId || currentMoveId || 'Draft move',
    topic,
    level,
    type,
    category: null,
    group,
    baseMove: null,
    components: null,
    parentIds,
    childIds,
    relatedMoveIds,
    positions,
    seeAlso: null,
    tags,
    description,
    source,
    comments,
    reviewFlag,
    reviewNotes,
    moveOrder: null,
    topicCol: null,
    topicOrder: null,
    familyOrder: null,
    valid: true,
    errors: null,
    hasLocalVideo: false,
    videoFiles: [],
    videoLinks: []
  };
  $: idCollisionMove = currentMoveId
    ? (
      selectedEditor?.kind === 'published'
        ? data.moves.find((move) => moveDisplayId(move) === currentDisplayId && move.id !== currentMoveId)
        : data.moves.find((move) => move.id === currentMoveId && selectedEditor?.id !== currentMoveId)
    )
    : null;
  $: idCollisionDraft = (selectedEditor?.kind === 'published' ? currentDisplayId : currentMoveId)
    ? (
      selectedEditor?.kind === 'published'
        ? drafts.find((draft) => moveDisplayId(draft.move) === currentDisplayId)
        : drafts.find((draft) => draft.move.id === currentMoveId && selectedEditor?.id !== draft.draftId)
    )
    : null;
  $: idCollisionWarning = (selectedEditor?.kind === 'published' ? currentDisplayId : currentMoveId) && (idCollisionMove || idCollisionDraft)
    ? `ID ${selectedEditor?.kind === 'published' ? currentDisplayId : currentMoveId} is already used by ${idCollisionMove?.name ?? idCollisionDraft?.move.name ?? 'another move'}.`
    : '';
  $: publishDisabledReason = selectedEditor?.kind === 'published'
    ? ''
    : !name.trim()
    ? 'Add a move name before publishing.'
    : !currentMoveId
      ? 'Add a move ID before publishing.'
      : idCollisionWarning
        ? idCollisionWarning
        : '';
  $: canPublishDraft = !publishDisabledReason;
  $: {
    selectedEditor;
    id;
    name;
    topic;
    level;
    type;
    group;
    positions;
    tags;
    source;
    description;
    comments;
    reviewFlag;
    reviewNotes;
    parentIds;
    childIds;
    relatedMoveIds;
    scheduleDraftAutosave();
  }

  function metadataOption(entry: MetadataEntry) {
    return {
      id: entry.id,
      label: entry.name,
      secondary: `${entry.moveCount} moves`
    };
  }

  function previewFromMove(move: MoveCardRecord | null): MovePreview | null {
    if (!move) return null;
    const filePath = move.previewVideoFile ?? move.videoFiles[0] ?? null;
    if (!filePath) return null;
    return {
      filePath,
      posterFile: move.posterFile,
      label: move.name ?? moveDisplayId(move)
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

  function currentDraftAutosaveSignature() {
    if (!selectedEditor || selectedEditor.kind === 'published') {
      return '';
    }
    return JSON.stringify({
      editor: selectedEditor,
      move: currentMovePayload()
    });
  }

  function clearAutosaveTimer() {
    if (!autosaveTimer) return;
    clearTimeout(autosaveTimer);
    autosaveTimer = null;
  }

  function resetDraftAutosaveBaseline() {
    clearAutosaveTimer();
    lastAutosaveSignature = currentDraftAutosaveSignature();
  }

  function existingMoveIdsForDraftId() {
    const selectedDraftId = selectedEditor?.kind === 'draft' ? selectedEditor.id : null;
    return [
      ...data.moves.map((move) => move.id),
      ...drafts.filter((draft) => draft.draftId !== selectedDraftId).map((draft) => draft.move.id)
    ];
  }

  function ensureDraftAutosaveId() {
    if (selectedEditor?.kind === 'published') return true;
    if (currentMoveId) return true;
    const generatedId = draftMoveIdFromName(name, existingMoveIdsForDraftId());
    if (!name.trim() || !generatedId) return false;
    id = generatedId;
    return true;
  }

  function scheduleDraftAutosave() {
    if (!browser || !selectedEditor || selectedEditor.kind === 'published') return;
    const signature = currentDraftAutosaveSignature();
    if (!signature || signature === lastAutosaveSignature) return;
    clearAutosaveTimer();
    autosaveTimer = setTimeout(() => {
      autosaveTimer = null;
      void autosaveDraft(signature);
    }, autosaveDelayMs);
  }

  async function autosaveDraft(expectedSignature: string) {
    if (!selectedEditor || selectedEditor.kind === 'published') return;
    if (expectedSignature !== currentDraftAutosaveSignature()) return;
    if (!ensureDraftAutosaveId() || idCollisionWarning) return;
    await saveDraft({ automatic: true });
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

  onMount(() => {
    if (!browser) return;
    try {
      const stored = JSON.parse(window.localStorage.getItem(pinnedStorageKey) ?? '[]');
      if (Array.isArray(stored)) {
        pinnedMoveIds = [...new Set([...data.recentMoveIds, ...stored.map((entry) => String(entry))])];
      }
    } catch {
      pinnedMoveIds = [...data.recentMoveIds];
    }
  });

  onDestroy(() => {
    clearAutosaveTimer();
  });

  $: if (browser) {
    window.localStorage.setItem(pinnedStorageKey, JSON.stringify(pinnedMoveIds));
  }

  function pinMove(moveId: string) {
    if (pinnedMoveIds.includes(moveId)) {
      return;
    }
    pinnedMoveIds = [moveId, ...pinnedMoveIds];
    pinSearch = '';
  }

  function removePinnedMove(moveId: string) {
    pinnedMoveIds = pinnedMoveIds.filter((entry) => entry !== moveId);
  }

  function clearPinnedMoves() {
    pinnedMoveIds = [];
    pinSearch = '';
  }

  function hasMoveSearchMatch(move: MoveCardRecord) {
    const query = pinSearch.trim().toLocaleLowerCase();
    if (!query) return false;
    return (
      moveDisplayId(move).toLocaleLowerCase().includes(query) ||
      move.id.toLocaleLowerCase().includes(query) ||
      move.slug.toLocaleLowerCase().includes(query) ||
      String(move.name ?? '').toLocaleLowerCase().includes(query)
    );
  }

  $: editorTitle =
    selectedEditor?.kind === 'published'
      ? 'Edit move'
      : 'Draft move';

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }

  function normalizeMoveId(value: string) {
    return value.trim().toUpperCase();
  }

  function loadMoveValues(move: MoveRecord) {
    id = moveDisplayId(move);
    name = move.name ?? '';
    topic = move.topic ?? '';
    level = move.level ?? '';
    type = move.type ?? '';
    group = move.group ?? '';
    positions = move.positions ?? '';
    tags = move.tags ?? '';
    source = move.source ?? '';
    description = move.description ?? '';
    comments = move.comments ?? '';
    reviewFlag = Boolean(move.reviewFlag);
    reviewNotes = move.reviewNotes ?? '';
    parentIds = [...move.parentIds];
    childIds = [...move.childIds];
    relatedMoveIds = [...move.relatedMoveIds];
    topicQuery = '';
    familyQuery = '';
    positionsQuery = '';
    tagsQuery = '';
    sourceQuery = '';
    isReviewOpen = reviewFlag;
    status = '';
  }

  function currentMovePayload() {
    return {
      id: selectedEditor?.kind === 'published' ? undefined : normalizeMoveId(id),
      displayId: normalizeMoveDisplayId(id),
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
  }

  function startNewDraft() {
    selectedEditor = { kind: 'new', id: null };
    id = '';
    name = '';
    topic = '';
    level = '';
    type = '';
    group = '';
    positions = '';
    tags = '';
    source = '';
    description = '';
    comments = '';
    reviewFlag = false;
    reviewNotes = '';
    parentIds = [];
    childIds = [];
    relatedMoveIds = [];
    topicQuery = '';
    familyQuery = '';
    positionsQuery = '';
    tagsQuery = '';
    sourceQuery = '';
    isReviewOpen = false;
    status = '';
    resetDraftAutosaveBaseline();
  }

  function loadDraft(draft: MoveDraft) {
    selectedEditor = { kind: 'draft', id: draft.draftId };
    loadMoveValues(draft.move);
    resetDraftAutosaveBaseline();
  }

  function loadPublishedMove(move: MoveRecord) {
    selectedEditor = { kind: 'published', id: move.id };
    loadMoveValues(move);
    resetDraftAutosaveBaseline();
  }

  function loadReviewMove(entry: ReviewMoveEntry) {
    if (entry.kind === 'draft') {
      loadDraft(entry.draft);
      return;
    }
    loadPublishedMove(entry.move);
  }

  function addConnection(kind: 'parent' | 'child' | 'related', selectedMoveId: string) {
    const moveId = normalizeMoveId(selectedMoveId);
    if (!moveId || moveId === currentMoveId || !data.moves.some((move) => move.id === moveId)) {
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

  function addConnectionFromPinned(moveId: string, actionId: string) {
    if (actionId === 'parent' || actionId === 'child' || actionId === 'related') {
      addConnection(actionId, moveId);
    }
  }

  async function saveDraft(options: { automatic?: boolean } = {}) {
    if (!ensureDraftAutosaveId() || idCollisionWarning) {
      return false;
    }

    isSaving = true;
    status = options.automatic ? 'Autosaving draft...' : 'Saving draft...';
    const response = await fetch('/api/moves/create', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        action: 'saveDraft',
        draftId: selectedEditor?.kind === 'draft' ? selectedEditor.id : undefined,
        move: currentMovePayload()
      })
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not save draft.';
      return false;
    }

    selectedEditor = { kind: 'draft', id: payload.draft.draftId };
    drafts = [payload.draft, ...drafts.filter((draft) => draft.draftId !== payload.draft.draftId)];
    lastAutosaveSignature = currentDraftAutosaveSignature();
    status = options.automatic ? 'Draft autosaved.' : 'Draft saved.';
    return true;
  }

  async function savePublishedMove() {
    if (selectedEditor?.kind !== 'published') {
      return;
    }

    isSaving = true;
    status = 'Saving move...';
    const response = await fetch(`/api/moves/${encodeURIComponent(selectedEditor.id)}`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(currentMovePayload())
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not save move.';
      return;
    }

    status = 'Move saved.';
    await invalidateAll();
    await goto(`/moves/${payload.move.slug}`);
  }

  async function deleteDraft() {
    if (selectedEditor?.kind !== 'draft') {
      return;
    }

    const draftId = selectedEditor.id;
    const draftLabel = name.trim() || id.trim() || 'this draft';
    if (browser && !window.confirm(`Delete draft "${draftLabel}"?`)) {
      return;
    }

    isSaving = true;
    status = 'Deleting draft...';
    const response = await fetch('/api/moves/create', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        action: 'deleteDraft',
        draftId
      })
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not delete draft.';
      return;
    }

    drafts = drafts.filter((draft) => draft.draftId !== draftId);
    selectedEditor = null;
    status = '';
    await invalidateAll();
  }

  async function publishDraft() {
    if (selectedEditor?.kind === 'published') {
      await savePublishedMove();
      return;
    }

    if (selectedEditor?.kind !== 'draft') {
      await saveDraft({ automatic: false });
    }
    if (selectedEditor?.kind !== 'draft') {
      return;
    }

    isSaving = true;
    status = 'Publishing...';
    const response = await fetch('/api/moves/create', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        action: 'publishDraft',
        draftId: selectedEditor.id,
        move: currentMovePayload()
      })
    });
    const payload = await response.json();
    isSaving = false;

    if (!response.ok) {
      status = payload.error ?? 'Could not publish move.';
      return;
    }

    await invalidateAll();
    await goto(`/moves/${payload.move.slug}`);
  }

  function toggleReviewFlag() {
    reviewFlag = !reviewFlag;
    isReviewOpen = reviewFlag;
  }
</script>

<svelte:head>
  <title>Create move | Salsa Encyclopedia</title>
</svelte:head>

<div class="move-create-page">
  <div class="move-editor-topbar">
    <a class="pill move-backlink" href="/moves/create/metadata">Topics and families</a>
  </div>
  <div class="move-create-layout">
    <aside class="panel meta-card move-draft-sidebar move-card-pane">
      <div class="panel-header">
        <div class="media-properties-title">
          <h3>Moves</h3>
          <span>{drafts.length} drafts · {pinnedMoves.length} pinned</span>
        </div>
      </div>

      <div class="move-card-section">
        <h4>Drafts</h4>
        <div class="move-card-grid">
          <button type="button" class="media-gallery-card move-gallery-card move-new-card" class:active={selectedEditor?.kind === 'new'} on:click={startNewDraft}>
            <span class="media-gallery-card-body">
              <strong>New move</strong>
            </span>
          </button>
          {#each drafts as draft}
            <button
              type="button"
              class="media-gallery-card move-gallery-card"
              class:active={selectedEditor?.kind === 'draft' && selectedEditor.id === draft.draftId}
              on:click={() => loadDraft(draft)}
            >
              <span class="media-gallery-poster move-gallery-poster">
                <span>Draft</span>
              </span>
              <span class="media-gallery-card-body">
                <strong>{draft.move.name ?? draft.move.id}</strong>
                <span>{moveDisplayId(draft.move)}</span>
                <span>{draft.move.parentIds.length} parents · {draft.move.childIds.length} children</span>
              </span>
            </button>
          {/each}
        </div>
      </div>

      <div class="move-card-section">
        <div class="move-card-section-heading">
          <h4>Review</h4>
          <button
            type="button"
            class="move-card-collapse-button"
            aria-label={isReviewListOpen ? 'Collapse review moves' : 'Expand review moves'}
            aria-expanded={isReviewListOpen}
            on:click={() => (isReviewListOpen = !isReviewListOpen)}
          >
            <span aria-hidden="true"></span>
          </button>
        </div>
        {#if isReviewListOpen && reviewMoveEntries.length}
          <div class="move-card-grid">
            {#each reviewMoveEntries as entry}
              <button
                type="button"
                class="media-gallery-card move-gallery-card review-move-card"
                class:active={(entry.kind === 'draft' && selectedEditor?.kind === 'draft' && selectedEditor.id === entry.draft.draftId) ||
                  (entry.kind === 'published' && selectedEditor?.kind === 'published' && selectedEditor.id === entry.move.id)}
                on:click={() => loadReviewMove(entry)}
              >
                <span class="media-gallery-card-body">
                  <strong>{entry.move.name ?? moveDisplayId(entry.move)}</strong>
                  <span>{entry.kind === 'draft' ? 'Draft' : moveDisplayId(entry.move)}</span>
                  {#if entry.move.reviewNotes}
                    <span>{entry.move.reviewNotes}</span>
                  {/if}
                </span>
              </button>
            {/each}
          </div>
        {:else if isReviewListOpen}
          <p class="move-card-empty">No moves marked</p>
        {/if}
      </div>

      <div class="move-card-section">
        <div class="move-card-section-heading">
          <h4>Pinned moves</h4>
          {#if pinnedMoves.length}
            <button type="button" on:click={clearPinnedMoves}>Clear</button>
          {/if}
        </div>
        <div class="move-pin-search">
          <MovePicker
            moves={data.moves}
            selectedIds={pinnedMoveIds}
            query={pinSearch}
            placeholder="Search moves to pin"
            ariaLabel="Search moves to pin"
            showSelected={false}
            showPoster={true}
            floatingDropdown={true}
            emptyText="No moves to pin"
            on:query={(event) => (pinSearch = event.detail.query)}
            on:select={(event) => pinMove(event.detail.moveId)}
          />
        </div>
        <div class="move-card-grid">
          {#each pinnedMoves as move}
            <div class="pinned-move-shell">
              <ContextMenu
                items={relationshipMenuItems}
                disabled={!selectedEditor}
                on:select={(event) => addConnectionFromPinned(move.id, event.detail.id)}
              >
                <button
                  type="button"
                  class="media-gallery-card move-gallery-card"
                  class:active={selectedEditor?.kind === 'published' && selectedEditor.id === move.id}
                  class:search-match={hasMoveSearchMatch(move)}
                  on:click={() => loadPublishedMove(move)}
                >
                  <span class="media-gallery-poster">
                    {#if move.posterFile}
                      <img src={posterUrl(move.posterFile)} alt="" loading="lazy" />
                    {:else}
                      <span>No preview</span>
                    {/if}
                  </span>
                  <span class="media-gallery-card-body">
                    <strong>{move.name ?? moveDisplayId(move)}</strong>
                    <span>{moveDisplayId(move)}</span>
                    <span>{move.videoFiles.length} videos · {move.childIds.length} children</span>
                  </span>
                </button>
              </ContextMenu>
              <button
                type="button"
                class="pinned-remove-button"
                aria-label={`Remove ${move.name ?? move.id} from pinned moves`}
                on:click={() => removePinnedMove(move.id)}
              >
                x
              </button>
            </div>
          {/each}
        </div>
      </div>
    </aside>

    <main class="move-create-main">
      {#if selectedEditor}
        <section class="panel meta-card move-editor-card move-editor-workspace">
          <header class="move-editor-hero">
            <div>
              <h2>{editorTitle}</h2>
            </div>
            {#if status}
              <span class="move-editor-status">{status}</span>
            {/if}
          </header>

          {#if selectedMovePreview}
            <div class="move-editor-section move-editor-preview-section">
              <MoveVideoPreview
                filePath={selectedMovePreview.filePath}
                posterFile={selectedMovePreview.posterFile}
                label={`${selectedMovePreview.label} video preview`}
              />
            </div>
          {/if}

          <div class="move-editor-section move-editor-identity">
            <label class="move-form-field move-form-name">
              <span>Name</span>
              <input bind:value={name} placeholder="Move name" />
            </label>
            <label class="move-form-field move-form-id">
              <span>ID</span>
              <input bind:value={id} autocapitalize="characters" placeholder="MOVE0001" />
              {#if idCollisionWarning}
                <span class="field-warning">{idCollisionWarning}</span>
              {/if}
            </label>
            <label class="move-form-field move-form-level">
              <span>Level</span>
              <select bind:value={level}>
                {#each levelOptions as option}
                  <option value={option}>{option || '—'}</option>
                {/each}
              </select>
            </label>
            <label class="move-form-field move-form-type move-form-type-mobile">
              <span>Type</span>
              <MoveTypeControl bind:value={type} />
            </label>
          </div>

          <div class="move-editor-section">
            <div class="move-detail-grid">
              <label class="move-form-field move-form-type move-form-type-desktop">
                <span>Type</span>
                <MoveTypeControl bind:value={type} />
              </label>
              <label class="move-form-field move-form-topic">
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
              <label class="move-form-field move-form-family">
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
              <label class="move-form-field move-form-positions">
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
              <label class="move-form-field move-form-tags">
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
              <label class="move-form-field move-form-authorship">
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
            </div>
          </div>

          <div class="move-editor-section">
            <div class="move-notes-grid">
              <label class="move-form-field">
                <span>Description</span>
                <AutoResizeTextarea bind:value={description} rows={1} />
              </label>
              <label class="move-form-field">
                <span>Comments</span>
                <AutoResizeTextarea bind:value={comments} rows={1} />
              </label>
            </div>
          </div>

          <div class="move-editor-section">
            <div class="move-section-heading">
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
          </div>

          <footer class="move-editor-actions" aria-label="Move editor actions">
            {#if selectedEditor.kind === 'published'}
              <button class="header-button" type="button" disabled={isSaving} on:click={savePublishedMove}>Save changes</button>
            {:else}
              <button
                class="header-button"
                type="button"
                disabled={isSaving || !canPublishDraft}
                title={publishDisabledReason}
                on:click={publishDraft}
              >
                Publish
              </button>
              {#if selectedEditor.kind === 'draft'}
                <button class="header-button danger-button" type="button" disabled={isSaving} on:click={deleteDraft}>Delete</button>
              {/if}
            {/if}
            <button
              class="header-button review-button"
              class:active={reviewFlag}
              type="button"
              aria-pressed={reviewFlag}
              disabled={isSaving}
              on:click={toggleReviewFlag}
            >
              {reviewFlag ? 'Marked for review' : 'Mark for review'}
            </button>
          </footer>

          {#if reviewFlag || isReviewOpen}
            <div class="move-editor-section move-review-section">
              <label class="move-form-field">
                <span>Review notes</span>
                <textarea bind:value={reviewNotes}></textarea>
              </label>
            </div>
          {/if}
        </section>
      {:else}
        <section class="panel meta-card move-editor-empty">
          <h3>Select a move</h3>
          <p class="muted">Choose a draft or published move from the left pane to open the editor.</p>
        </section>
      {/if}
    </main>
  </div>
</div>
