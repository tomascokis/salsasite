<script lang="ts">
  import { browser } from '$app/environment';
  import { goto, invalidateAll } from '$app/navigation';
  import { onMount } from 'svelte';
  import ContextMenu from '$lib/components/ContextMenu.svelte';
  import MovePicker from '$lib/components/MovePicker.svelte';
  import SearchablePicker from '$lib/components/SearchablePicker.svelte';
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

  type SelectedEditor =
    | { kind: 'draft'; id: string }
    | { kind: 'published'; id: string }
    | { kind: 'new'; id: null }
    | null;

  export let data: {
    moves: MoveCardRecord[];
    drafts: MoveDraft[];
    metadata: SiteMetadata;
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
  let parentDraft = '';
  let childDraft = '';
  let relatedDraft = '';
  let topicQuery = '';
  let familyQuery = '';
  let status = '';
  let isSaving = false;
  let pinSearch = '';
  let pinnedMoveIds = [...data.recentMoveIds];
  let isReviewOpen = false;
  let isCardPaneMinimized = false;
  let lastWindowScrollY = 0;
  const pinnedStorageKey = 'salsa-encyclopedia:pinned-create-moves';
  const typeOptions = ['Addition', 'Variation', ''];
  const levelOptions = ['', '1', '2', '3', '4', '5'];
  const relationshipMenuItems = [
    { id: 'parent', label: 'Add as parent' },
    { id: 'child', label: 'Add as child' },
    { id: 'related', label: 'Add as related' }
  ];

  $: pinnedMoves = pinnedMoveIds
    .map((moveId) => data.moves.find((move) => move.id === moveId))
    .filter((move): move is MoveCardRecord => Boolean(move));
  $: topicOptions = data.metadata.topics.map(metadataOption);
  $: familyOptions = data.metadata.families.map(metadataOption);
  $: selectedTopicIds = topic ? matchingMetadataIds(data.metadata.topics, topic) : [];
  $: selectedFamilyIds = group ? matchingMetadataIds(data.metadata.families, group) : [];
  $: idCollisionMove = currentMoveId
    ? data.moves.find((move) => move.id === currentMoveId && selectedEditor?.id !== currentMoveId)
    : null;
  $: idCollisionDraft = currentMoveId
    ? drafts.find((draft) => draft.move.id === currentMoveId && selectedEditor?.id !== draft.draftId)
    : null;
  $: idCollisionWarning = currentMoveId && (idCollisionMove || idCollisionDraft)
    ? `ID ${currentMoveId} is already used by ${idCollisionMove?.name ?? idCollisionDraft?.move.name ?? 'another move'}.`
    : '';

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

  function handlePageScroll() {
    if (!browser) return;
    if (window.innerWidth > 720) {
      isCardPaneMinimized = false;
      lastWindowScrollY = window.scrollY;
      return;
    }

    const nextScrollY = window.scrollY;
    if (nextScrollY <= 4) {
      isCardPaneMinimized = false;
      lastWindowScrollY = nextScrollY;
      return;
    }

    const delta = nextScrollY - lastWindowScrollY;
    if (Math.abs(delta) < 8) {
      return;
    }

    if (delta > 0 && nextScrollY > 120) {
      isCardPaneMinimized = true;
    }
    lastWindowScrollY = nextScrollY;
  }

  function hasMoveSearchMatch(move: MoveCardRecord) {
    const query = pinSearch.trim().toLocaleLowerCase();
    if (!query) return false;
    return (
      move.id.toLocaleLowerCase().includes(query) ||
      move.slug.toLocaleLowerCase().includes(query) ||
      String(move.name ?? '').toLocaleLowerCase().includes(query)
    );
  }

  $: editorTitle =
    selectedEditor?.kind === 'published'
      ? 'Edit move'
      : selectedEditor?.kind === 'draft'
        ? 'Edit draft'
        : 'Create move';

  $: editorSubtitle =
    selectedEditor?.kind === 'published'
      ? id
      : selectedEditor?.kind === 'draft'
        ? 'Draft move'
        : 'New draft';

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }

  function normalizeMoveId(value: string) {
    return value.trim().toUpperCase();
  }

  $: currentMoveId = normalizeMoveId(id);

  function loadMoveValues(move: MoveRecord) {
    id = move.id;
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
    parentDraft = '';
    childDraft = '';
    relatedDraft = '';
    topicQuery = '';
    familyQuery = '';
    isReviewOpen = reviewFlag;
    status = '';
  }

  function currentMovePayload() {
    return {
      id,
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
    parentDraft = '';
    childDraft = '';
    relatedDraft = '';
    topicQuery = '';
    familyQuery = '';
    isReviewOpen = false;
    status = '';
  }

  function loadDraft(draft: MoveDraft) {
    selectedEditor = { kind: 'draft', id: draft.draftId };
    loadMoveValues(draft.move);
  }

  function loadPublishedMove(move: MoveRecord) {
    selectedEditor = { kind: 'published', id: move.id };
    loadMoveValues(move);
  }

  function addConnection(kind: 'parent' | 'child' | 'related', selectedMoveId?: string) {
    const draft = selectedMoveId ?? (kind === 'parent' ? parentDraft : kind === 'child' ? childDraft : relatedDraft);
    const moveId = normalizeMoveId(draft);
    if (!moveId || moveId === currentMoveId || !data.moves.some((move) => move.id === moveId)) {
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

  function addConnectionFromPinned(moveId: string, actionId: string) {
    if (actionId === 'parent' || actionId === 'child' || actionId === 'related') {
      addConnection(actionId, moveId);
    }
  }

  async function saveDraft() {
    isSaving = true;
    status = 'Saving draft...';
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
      return;
    }

    selectedEditor = { kind: 'draft', id: payload.draft.draftId };
    drafts = [payload.draft, ...drafts.filter((draft) => draft.draftId !== payload.draft.draftId)];
    status = 'Draft saved.';
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

  async function publishDraft() {
    if (selectedEditor?.kind === 'published') {
      await savePublishedMove();
      return;
    }

    if (selectedEditor?.kind !== 'draft') {
      await saveDraft();
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
</script>

<svelte:head>
  <title>Create move | Salsa Encyclopedia</title>
</svelte:head>

<svelte:window on:scroll={handlePageScroll} on:resize={handlePageScroll} />

<div class="move-create-page">
  <div class="move-editor-topbar">
    <a class="pill move-backlink" href="/moves/create/metadata">Topics and families</a>
  </div>
  <div class="move-create-layout">
    <aside class="panel meta-card move-draft-sidebar move-card-pane" class:minimized={isCardPaneMinimized}>
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
                <span>{draft.move.id}</span>
                <span>{draft.move.parentIds.length} parents · {draft.move.childIds.length} children</span>
              </span>
            </button>
          {/each}
        </div>
      </div>

      {#if selectedEditor}
        <div class="move-card-section review-sidebar-section">
          <button type="button" class="review-toggle" on:click={() => (isReviewOpen = !isReviewOpen)}>
            <span>Review</span>
            <span>{reviewFlag ? 'Flagged' : 'Hidden'}</span>
          </button>
          {#if isReviewOpen}
            <label class="review-checkbox">
              <input type="checkbox" bind:checked={reviewFlag} />
              <span>Flag this move</span>
            </label>
            <label class="move-form-field">
              <span>Review notes</span>
              <textarea bind:value={reviewNotes}></textarea>
            </label>
          {/if}
        </div>
      {/if}

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
                    <strong>{move.name ?? move.id}</strong>
                    <span>{move.id}</span>
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
              <span class="move-editor-kicker">{editorSubtitle}</span>
              <h2>{name || editorTitle}</h2>
            </div>
            {#if status}
              <span class="move-editor-status">{status}</span>
            {/if}
          </header>

          <div class="move-editor-section move-editor-identity">
            <label class="move-form-field move-form-name">
              <span>Name</span>
              <input bind:value={name} placeholder="Move name" />
            </label>
            <label class="move-form-field">
              <span>ID</span>
              <input bind:value={id} autocapitalize="characters" disabled={selectedEditor.kind === 'published'} placeholder="MOVE0001" />
              {#if idCollisionWarning}
                <span class="field-warning">{idCollisionWarning}</span>
              {/if}
            </label>
            <label class="move-form-field move-form-level">
              <span>Level</span>
              <select bind:value={level}>
                {#each levelOptions as option}
                  <option value={option}>{option || 'Unspecified'}</option>
                {/each}
              </select>
            </label>
          </div>

          <div class="move-editor-section">
            <div class="move-section-heading">
              <h3>Details</h3>
            </div>
            <div class="move-detail-grid">
              <label class="move-form-field">
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
              <label class="move-form-field">
                <span>Type</span>
                <span class="segmented-control move-segmented-control">
                  {#each typeOptions as option}
                    <button type="button" class:active={type === option} on:click={() => (type = option)}>
                      {option || 'Neither'}
                    </button>
                  {/each}
                </span>
              </label>
              <label class="move-form-field">
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
              <label class="move-form-field">
                <span>Positions</span>
                <input bind:value={positions} />
              </label>
              <label class="move-form-field">
                <span>Tags</span>
                <input bind:value={tags} />
              </label>
              <label class="move-form-field">
                <span>Source</span>
                <input bind:value={source} />
              </label>
            </div>
          </div>

          <div class="move-editor-section">
            <div class="move-section-heading">
              <h3>Notes</h3>
            </div>
            <div class="move-notes-grid">
              <label class="move-form-field">
                <span>Description</span>
                <textarea bind:value={description}></textarea>
              </label>
              <label class="move-form-field">
                <span>Comments</span>
                <textarea bind:value={comments}></textarea>
              </label>
            </div>
          </div>

          <div class="move-editor-section">
            <div class="move-section-heading">
              <h3>Connections</h3>
            </div>

            <div class="connection-editor-grid">
              <div class="connection-editor">
                <h4>Parents</h4>
                <MovePicker
                  moves={data.moves}
                  selectedIds={parentIds}
                  excludedIds={currentMoveId ? [currentMoveId] : []}
                  query={parentDraft}
                  on:query={(event) => updateConnectionQuery('parent', event.detail.query)}
                  on:select={(event) => addConnection('parent', event.detail.moveId)}
                  on:remove={(event) => removeConnection('parent', event.detail.moveId)}
                />
              </div>

              <div class="connection-editor">
                <h4>Children</h4>
                <MovePicker
                  moves={data.moves}
                  selectedIds={childIds}
                  excludedIds={currentMoveId ? [currentMoveId] : []}
                  query={childDraft}
                  on:query={(event) => updateConnectionQuery('child', event.detail.query)}
                  on:select={(event) => addConnection('child', event.detail.moveId)}
                  on:remove={(event) => removeConnection('child', event.detail.moveId)}
                />
              </div>

              <div class="connection-editor">
                <h4>Related moves</h4>
                <MovePicker
                  moves={data.moves}
                  selectedIds={relatedMoveIds}
                  excludedIds={currentMoveId ? [currentMoveId] : []}
                  query={relatedDraft}
                  on:query={(event) => updateConnectionQuery('related', event.detail.query)}
                  on:select={(event) => addConnection('related', event.detail.moveId)}
                  on:remove={(event) => removeConnection('related', event.detail.moveId)}
                />
              </div>
            </div>
          </div>

          <footer class="move-editor-actions" aria-label="Move editor actions">
            {#if selectedEditor.kind === 'published'}
              <button class="header-button" type="button" disabled={isSaving} on:click={savePublishedMove}>Save changes</button>
            {:else}
              <button class="header-button" type="button" disabled={isSaving} on:click={publishDraft}>Publish</button>
            {/if}
          </footer>
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
