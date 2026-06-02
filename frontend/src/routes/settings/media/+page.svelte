<script lang="ts">
  type MediaFileAction = {
    id: string;
    actionType: string;
    status: 'planned' | 'running' | 'succeeded' | 'failed';
    filePath: string;
    backupPath: string | null;
    metadata: unknown;
    createdAt: string;
    updatedAt: string;
  };

  type MediaJob = {
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
    fileActions?: MediaFileAction[];
    fileActionSummary?: {
      total: number;
      failed: number;
      missing: number;
      byActionType: Record<string, number>;
    };
  };

  export let data: {
    jobs: MediaJob[];
  };

  let jobs = data.jobs;
  let statusMessage = '';
  let busyJobId: string | null = null;
  let backfillBusy = false;
  let expandedJobIds = new Set<string>();

  function formatDate(value: string) {
    const date = new Date(value);
    return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
  }

  function canRetry(job: { status: string; type: string }) {
    return job.status === 'failed' && ['clip.render', 'poster.generate', 'source.hash'].includes(job.type);
  }

  function toggleJobDetails(id: string) {
    const next = new Set(expandedJobIds);
    if (next.has(id)) {
      next.delete(id);
    } else {
      next.add(id);
    }
    expandedJobIds = next;
  }

  function metadataEntries(metadata: unknown) {
    if (!metadata || typeof metadata !== 'object' || Array.isArray(metadata)) {
      return [];
    }
    return Object.entries(metadata as Record<string, unknown>).map(([key, value]) => ({
      key,
      value: typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean'
        ? String(value)
        : JSON.stringify(value)
    }));
  }

  function isMissingAction(action: MediaFileAction) {
    return Boolean(
      action.metadata &&
        typeof action.metadata === 'object' &&
        (action.metadata as Record<string, unknown>).missing === true
    );
  }

  function fileCountLabel(job: MediaJob) {
    const summary = job.fileActionSummary;
    if (!summary || summary.total === 0) {
      return '0';
    }
    const parts = [`${summary.total}`];
    if (summary.failed > 0) {
      parts.push(`${summary.failed} failed`);
    }
    if (summary.missing > 0) {
      parts.push(`${summary.missing} skipped`);
    }
    return parts.join(' / ');
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

<div class="settings-page media-manager-page">
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
              <th>Files</th>
              <th>Error</th>
              <th>Details</th>
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
                  <strong>{fileCountLabel(job)}</strong>
                  {#if job.fileActionSummary && Object.keys(job.fileActionSummary.byActionType).length > 0}
                    <span>{Object.entries(job.fileActionSummary.byActionType).map(([type, count]) => `${type}: ${count}`).join(', ')}</span>
                  {/if}
                </td>
                <td>
                  {#if job.error}
                    <span>{job.error}</span>
                  {:else}
                    <span class="muted">None</span>
                  {/if}
                </td>
                <td>
                  <button
                    type="button"
                    aria-expanded={expandedJobIds.has(job.id)}
                    on:click={() => toggleJobDetails(job.id)}
                  >
                    {expandedJobIds.has(job.id) ? 'Hide' : 'Details'}
                  </button>
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
              {#if expandedJobIds.has(job.id)}
                <tr class="media-job-detail-row">
                  <td colspan="8">
                    {#if !job.fileActions || job.fileActions.length === 0}
                      <p class="muted settings-status">No file actions recorded.</p>
                    {:else}
                      <table class="media-file-actions-table">
                        <thead>
                          <tr>
                            <th>When</th>
                            <th>File action</th>
                            <th>Status</th>
                            <th>File path</th>
                            <th>Backup / destination</th>
                            <th>Details</th>
                          </tr>
                        </thead>
                        <tbody>
                          {#each job.fileActions as action}
                            <tr>
                              <td>{formatDate(action.updatedAt)}</td>
                              <td>{action.actionType}</td>
                              <td>
                                <strong>{action.status}</strong>
                                {#if isMissingAction(action)}
                                  <span>Missing/skipped</span>
                                {/if}
                              </td>
                              <td><span>{action.filePath}</span></td>
                              <td>
                                {#if action.backupPath}
                                  <span>{action.backupPath}</span>
                                {:else}
                                  <span class="muted">None</span>
                                {/if}
                              </td>
                              <td>
                                {#if metadataEntries(action.metadata).length === 0}
                                  <span class="muted">None</span>
                                {:else}
                                  {#each metadataEntries(action.metadata) as entry}
                                    <span><strong>{entry.key}:</strong> {entry.value}</span>
                                  {/each}
                                {/if}
                              </td>
                            </tr>
                          {/each}
                        </tbody>
                      </table>
                    {/if}
                  </td>
                </tr>
              {/if}
            {/each}
          </tbody>
        </table>
      </div>
    {/if}
  </section>
</div>

<style>
  .media-manager-page {
    max-width: min(1180px, calc(100vw - 2rem));
  }

  .media-jobs-table-wrap {
    overflow-x: auto;
  }

  .media-jobs-table {
    width: 100%;
    border-collapse: collapse;
    min-width: 1120px;
  }

  .media-jobs-table th,
  .media-jobs-table td,
  .media-file-actions-table th,
  .media-file-actions-table td {
    padding: 0.7rem 0.75rem;
    border-bottom: 1px solid rgba(15, 23, 42, 0.12);
    text-align: left;
    vertical-align: top;
  }

  .media-jobs-table th,
  .media-file-actions-table th {
    font-size: 0.78rem;
    text-transform: uppercase;
    color: var(--muted);
  }

  .media-jobs-table td span,
  .media-file-actions-table td span {
    display: block;
    max-width: 28rem;
    margin-top: 0.15rem;
    overflow-wrap: anywhere;
    font-size: 0.82rem;
    color: var(--muted);
  }

  .media-job-detail-row td {
    background: #f8fafc;
  }

  .media-file-actions-table {
    width: 100%;
    border-collapse: collapse;
    min-width: 920px;
  }
</style>
