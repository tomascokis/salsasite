<script lang="ts">
  export let data: {
    entries: Array<{
      id: string;
      type: string;
      label: string;
      entityType: string;
      entityId: string;
      status: 'active' | 'undone';
      createdAt: string;
      canUndo: boolean;
      undoUnavailableReason: string | null;
    }>;
  };

  let entries = data.entries;
  let statusMessage = '';
  let busyActionId: string | null = null;

  function formatDate(value: string) {
    const date = new Date(value);
    return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
  }

  async function refreshHistory() {
    const response = await fetch('/api/history');
    const payload = await response.json();
    entries = payload.entries ?? entries;
  }

  async function undoEntry(id: string) {
    busyActionId = id;
    statusMessage = '';
    try {
      const response = await fetch(`/api/history/${encodeURIComponent(id)}/undo`, {
        method: 'POST'
      });
      const payload = await response.json();
      if (!response.ok) {
        throw new Error(payload.error ?? 'Could not undo action.');
      }
      statusMessage = 'Action undone';
      await refreshHistory();
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not undo action.';
    } finally {
      busyActionId = null;
    }
  }
</script>

<svelte:head>
  <title>Action History | Salsa Encyclopedia</title>
</svelte:head>

<div class="settings-page">
  <section class="settings-panel">
    <div class="settings-header">
      <h2>Action history</h2>
      <a class="header-button" href="/settings">Settings</a>
    </div>
    <p class="muted settings-status">
      Data-only changes can be undone here. Media file operations are recorded later.
    </p>
    {#if statusMessage}
      <p class="muted settings-status">{statusMessage}</p>
    {/if}
  </section>

  <section class="settings-panel">
    {#if entries.length === 0}
      <p class="muted settings-status">No recorded actions yet.</p>
    {:else}
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>When</th>
              <th>Action</th>
              <th>Target</th>
              <th>Status</th>
              <th>Undo</th>
            </tr>
          </thead>
          <tbody>
            {#each entries as entry}
              <tr>
                <td>{formatDate(entry.createdAt)}</td>
                <td>
                  <strong>{entry.label}</strong>
                  <span>{entry.type}</span>
                </td>
                <td>
                  <span>{entry.entityType}</span>
                  <span>{entry.entityId}</span>
                </td>
                <td>{entry.status}</td>
                <td>
                  {#if entry.canUndo}
                    <button type="button" disabled={busyActionId === entry.id} on:click={() => undoEntry(entry.id)}>
                      {busyActionId === entry.id ? 'Undoing' : 'Undo'}
                    </button>
                  {:else}
                    <span class="muted">{entry.undoUnavailableReason}</span>
                  {/if}
                </td>
              </tr>
            {/each}
          </tbody>
        </table>
      </div>
    {/if}
  </section>
</div>

<style>
  .history-table-wrap {
    overflow-x: auto;
  }

  .history-table {
    width: 100%;
    border-collapse: collapse;
    min-width: 760px;
  }

  .history-table th,
  .history-table td {
    padding: 0.7rem 0.75rem;
    border-bottom: 1px solid rgba(15, 23, 42, 0.12);
    text-align: left;
    vertical-align: top;
  }

  .history-table th {
    font-size: 0.78rem;
    text-transform: uppercase;
    color: var(--muted);
  }

  .history-table td span {
    display: block;
    margin-top: 0.15rem;
    font-size: 0.82rem;
    color: var(--muted);
  }
</style>

