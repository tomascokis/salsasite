<script lang="ts">
  import type { LayoutColumn, ProgressEntry, ProgressSnapshot } from '$lib/types';
  import { colorForProgress } from '$lib/progress-ui';

  export let data: {
    layout: LayoutColumn[];
    snapshot: ProgressSnapshot | null;
  };

  const prepOptions = ['Off the cards', 'Needs guidance', 'Not prepped', 'Shaky', 'Prepped'];
  const sequOptions = ['Not needed', 'Unused', 'Underused', 'Overused', 'Good', 'Great'];
  const succOptions = ['NA', 'Failure', '50-50', 'Nearly there', 'Great'];

  const initialEntries = Object.fromEntries(
    (data.snapshot?.entries ?? []).map((entry) => [entry.id, { ...entry }])
  ) as Record<string, ProgressEntry>;

  let entries = initialEntries;
  let selectedId: string | null = null;
  let changeCount = 0;

  const orderedLayout = data.layout
    .map((column) => ({
      ...column,
      entries: [...column.entries].sort((a, b) => (a.layoutOrder ?? 0) - (b.layoutOrder ?? 0))
    }))
    .sort((a, b) => a.column - b.column);

  const orderedIds = orderedLayout
    .flatMap((column) => column.entries)
    .filter((entry) => entry.entryType === 'Data' && entry.id)
    .map((entry) => entry.id as string);

  function touch() {
    changeCount += 1;
  }

  function ensureEntry(id: string) {
    if (!entries[id]) {
      entries[id] = {
        id,
        preparation: null,
        sequencing: null,
        success: null,
        statNumPrep: null,
        statNumSequ: null,
        statNumSucc: null
      };
    }

    return entries[id];
  }

  function cycleValue(id: string, dimension: 'prep' | 'sequ' | 'succ', direction: 1 | -1 = 1) {
    const entry = ensureEntry(id);

    if (dimension === 'prep') {
      const current = entry.statNumPrep ?? 0;
      entry.statNumPrep = Math.max(0, Math.min(4, current + direction));
      entry.preparation = prepOptions[entry.statNumPrep] ?? null;
    } else if (dimension === 'sequ') {
      const current = entry.statNumSequ ?? 0;
      entry.statNumSequ = Math.max(0, Math.min(5, current + direction));
      entry.sequencing = sequOptions[entry.statNumSequ] ?? null;
    } else {
      const current = entry.statNumSucc ?? 0;
      entry.statNumSucc = Math.max(0, Math.min(4, current + direction));
      entry.success = succOptions[entry.statNumSucc] ?? null;
    }

    entries = { ...entries };
    touch();
  }

  function moveSelection(delta: number) {
    if (!orderedIds.length) return;
    if (!selectedId) {
      selectedId = orderedIds[0];
      return;
    }

    const currentIndex = orderedIds.indexOf(selectedId);
    const nextIndex = Math.max(0, Math.min(orderedIds.length - 1, currentIndex + delta));
    selectedId = orderedIds[nextIndex];
  }

  function toCsv() {
    const rows = orderedIds.map((id) => {
      const entry = ensureEntry(id);
      return [
        data.snapshot?.date ?? '',
        id,
        entry.preparation ?? '',
        entry.sequencing ?? '',
        entry.success ?? '',
        entry.statNumPrep ?? '',
        entry.statNumSequ ?? '',
        entry.statNumSucc ?? ''
      ];
    });

    return [
      ['Date', 'ID', 'Preperation', 'Sequencing', 'Success', 'StatNum_Prep', 'StatNum_Sequ', 'StatNum_Succ'],
      ...rows
    ]
      .map((row) => row.join(','))
      .join('\n');
  }

  function exportCsv() {
    const blob = new Blob([toCsv()], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `progress-${data.snapshot?.date ?? 'snapshot'}.csv`;
    link.click();
    URL.revokeObjectURL(url);
  }

  async function importCsv(event: Event) {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    const text = await file.text();
    const lines = text.split(/\r?\n/).filter(Boolean);
    const next = { ...entries };

    for (const line of lines.slice(1)) {
      const [, id, preparation, sequencing, success, prep, sequ, succ] = line.split(',');
      if (!id) continue;
      next[id] = {
        id,
        preparation: preparation || null,
        sequencing: sequencing || null,
        success: success || null,
        statNumPrep: prep ? Number(prep) : null,
        statNumSequ: sequ ? Number(sequ) : null,
        statNumSucc: succ ? Number(succ) : null
      };
    }

    entries = next;
    touch();
    input.value = '';
  }

  function handleKey(event: KeyboardEvent) {
    if (event.target instanceof HTMLInputElement) return;

    const key = event.key.toLowerCase();
    if (key === 'arrowup' || key === 'w') {
      event.preventDefault();
      moveSelection(-1);
      return;
    }
    if (key === 'arrowdown' || key === 'r') {
      event.preventDefault();
      moveSelection(1);
      return;
    }
    if (!selectedId) return;

    if (key === 'l') cycleValue(selectedId, 'prep', 1);
    if (key === 'n') cycleValue(selectedId, 'prep', -1);
    if (key === 'u') cycleValue(selectedId, 'sequ', 1);
    if (key === 'e') cycleValue(selectedId, 'sequ', -1);
    if (key === 'y') cycleValue(selectedId, 'succ', 1);
    if (key === 'i') cycleValue(selectedId, 'succ', -1);
  }

  function isEmphasis(type: string | null) {
    return type === 'Addition' || type === 'Variation';
  }

  function typeClass(type: string | null) {
    if (type === 'Addition') return 'addition';
    if (type === 'Variation') return 'variation';
    return '';
  }

  function entryFor(id: string) {
    return entries[id] ?? {
      id,
      preparation: null,
      sequencing: null,
      success: null,
      statNumPrep: null,
      statNumSequ: null,
      statNumSucc: null
    };
  }
</script>

<svelte:window on:keydown={handleKey} />

<div class="stack">
  <section class="panel">
    <div class="panel-header">
      <h2>Progress Editor</h2>
      <p class="muted">
        Local-browser editing with CSV import/export. This replaces the static HTML editor and is designed
        to become API-backed later.
      </p>
    </div>
    <div style="padding: 1rem 1.1rem" class="stack">
      <div class="toolbar">
        <span class="pill">{changeCount} local changes</span>
        <span class="pill">Snapshot: {data.snapshot?.date ?? 'n/a'}</span>
        <button class="primary" on:click={exportCsv}>Export CSV</button>
        <label class="button-like">
          Import CSV
          <input hidden type="file" accept=".csv" on:change={importCsv} />
        </label>
      </div>

      <div class="dashboard-columns">
        {#each orderedLayout as column (column.column)}
          <div class="dashboard-column">
            {#each column.entries as row (row.layoutOrder ?? row.id ?? row.name)}
              {#if row.entryType === 'Title'}
                <div class="editor-row title-row">
                  <div class="title-cell">{row.name}</div>
                </div>
              {:else if row.id}
                {@const progress = entryFor(row.id)}
                <div
                  class={`editor-row data-row ${typeClass(row.type)} ${selectedId === row.id ? 'editor-selected' : ''}`}
                  role="button"
                  tabindex="0"
                  on:click={() => (selectedId = row.id ?? null)}
                  on:dblclick={() => row.slug && (window.location.href = `/moves/${row.slug}`)}
                >
                  <div class="level-cell">{row.level ?? ''}</div>
                  <div class={`name-cell ${isEmphasis(row.type) ? 'emphasis' : ''}`}>{row.name}</div>
                  <div
                    class="editor-dot"
                    style={`background:${colorForProgress('prep', progress.statNumPrep)}`}
                    on:click|stopPropagation={() => cycleValue(row.id as string, 'prep', 1)}
                  ></div>
                  <div
                    class="editor-dot"
                    style={`background:${colorForProgress('sequ', progress.statNumSequ)}`}
                    on:click|stopPropagation={() => cycleValue(row.id as string, 'sequ', 1)}
                  ></div>
                  <div
                    class="editor-dot"
                    style={`background:${colorForProgress('succ', progress.statNumSucc)}`}
                    on:click|stopPropagation={() => cycleValue(row.id as string, 'succ', 1)}
                  ></div>
                </div>
              {/if}
            {/each}
          </div>
        {/each}
      </div>

      <div class="panel" style="padding: 1rem">
        <div class="editor-legend">
          <p><strong>Navigation:</strong> <span class="kbd">↑</span>/<span class="kbd">↓</span> or <span class="kbd">W</span>/<span class="kbd">R</span></p>
          <p><strong>Preparation:</strong> <span class="kbd">L</span>/<span class="kbd">N</span></p>
          <p><strong>Sequencing:</strong> <span class="kbd">U</span>/<span class="kbd">E</span></p>
          <p><strong>Success:</strong> <span class="kbd">Y</span>/<span class="kbd">I</span></p>
        </div>
      </div>
    </div>
  </section>
</div>
