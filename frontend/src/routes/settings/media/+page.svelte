<script lang="ts">
  export let data: {
    jobs: Array<{
      id: string;
      type: string;
      targetType: string;
      targetId: string;
      status: 'queued' | 'running' | 'succeeded' | 'failed' | 'cancelled';
      attempts: number;
      maxAttempts: number;
      error: string | null;
      updatedAt: string;
      createdAt: string;
    }>;
  };

  let jobs = data.jobs;
  let statusMessage = '';
  let busyJobId: string | null = null;
  let backfillBusy = false;

  function formatDate(value: string) {
    const date = new Date(value);
    return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
  }

  function canRetry(job: { status: string; type: string }) {
    return job.status === 'failed' && ['clip.render', 'poster.generate', 'source.hash'].includes(job.type);
  }

  async function refreshJobs() {
    const response = await fetch('/api/media/jobs?limit=100');
    const payload = await response.json();
    jobs = payload.jobs ?? jobs;
  }

  async function queueHashBackfill() {
    backfillBusy = true;
    statusMessage = '';
    try {
      const response = await fetch('/api/media/jobs', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'source.hash.backfill' })
      });
      const payload = await response.json();
      if (!response.ok) {
        throw new Error(payload.error ?? 'Could not queue source hash backfill.');
      }
      statusMessage = `Queued ${payload.queued ?? 0} source hash job${payload.queued === 1 ? '' : 's'}.`;
      await refreshJobs();
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not queue source hash backfill.';
    } finally {
      backfillBusy = false;
    }
  }

  async function retryJob(id: string) {
    busyJobId = id;
    statusMessage = '';
    try {
      const response = await fetch(`/api/media/jobs/${encodeURIComponent(id)}/retry`, {
        method: 'POST'
      });
      const payload = await response.json();
      if (!response.ok) {
        throw new Error(payload.error ?? 'Could not retry media job.');
      }
      statusMessage = 'Media job queued.';
      await refreshJobs();
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not retry media job.';
    } finally {
      busyJobId = null;
    }
  }
</script>

<svelte:head>
  <title>Media Manager | Salsa Encyclopedia</title>
</svelte:head>

<div class="settings-page">
  <section class="settings-panel">
    <div class="settings-header">
      <h2>Media manager</h2>
      <span class="settings-header-actions">
        <button type="button" disabled={backfillBusy} on:click={queueHashBackfill}>
          {backfillBusy ? 'Queuing' : 'Backfill source hashes'}
        </button>
        <a class="header-button" href="/settings/history">Action history</a>
        <a class="header-button" href="/settings">Settings</a>
      </span>
    </div>
    <p class="muted settings-status">
      Media jobs track hashing, poster generation, clip rendering, and managed file operations.
    </p>
    {#if statusMessage}
      <p class="muted settings-status">{statusMessage}</p>
    {/if}
  </section>

  <section class="settings-panel">
    {#if jobs.length === 0}
      <p class="muted settings-status">No media jobs have been recorded yet.</p>
    {:else}
      <div class="media-jobs-table-wrap">
        <table class="media-jobs-table">
          <thead>
            <tr>
              <th>Updated</th>
              <th>Job</th>
              <th>Target</th>
              <th>Status</th>
              <th>Error</th>
              <th>Action</th>
            </tr>
          </thead>
          <tbody>
            {#each jobs as job}
              <tr>
                <td>{formatDate(job.updatedAt)}</td>
                <td>
                  <strong>{job.type}</strong>
                  <span>{job.id}</span>
                </td>
                <td>
                  <span>{job.targetType}</span>
                  <span>{job.targetId}</span>
                </td>
                <td>
                  <strong>{job.status}</strong>
                  <span>{job.attempts}/{job.maxAttempts}</span>
                </td>
                <td>
                  {#if job.error}
                    <span>{job.error}</span>
                  {:else}
                    <span class="muted">None</span>
                  {/if}
                </td>
                <td>
                  {#if canRetry(job)}
                    <button type="button" disabled={busyJobId === job.id} on:click={() => retryJob(job.id)}>
                      {busyJobId === job.id ? 'Retrying' : 'Retry'}
                    </button>
                  {:else}
                    <span class="muted">No action</span>
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
  .media-jobs-table-wrap {
    overflow-x: auto;
  }

  .media-jobs-table {
    width: 100%;
    border-collapse: collapse;
    min-width: 920px;
  }

  .media-jobs-table th,
  .media-jobs-table td {
    padding: 0.7rem 0.75rem;
    border-bottom: 1px solid rgba(15, 23, 42, 0.12);
    text-align: left;
    vertical-align: top;
  }

  .media-jobs-table th {
    font-size: 0.78rem;
    text-transform: uppercase;
    color: var(--muted);
  }

  .media-jobs-table td span {
    display: block;
    max-width: 28rem;
    margin-top: 0.15rem;
    overflow-wrap: anywhere;
    font-size: 0.82rem;
    color: var(--muted);
  }
</style>
