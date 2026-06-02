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

  type MediaRepairSummary = {
    missingVariantPathsPruned: number;
    legacyMoveAssetsAdded: number;
    legacyLinksAdded: number;
    staleGeneratedAssetsRemoved: number;
    staleGeneratedFilesTrashed: number;
    orphanDraftMoveIdsRelinked: number;
    generatedRenameActions: number;
    generatedVariantLinksRemoved: number;
  };

  type MediaRepairResult = {
    dryRun: boolean;
    changed: boolean;
    summary: MediaRepairSummary;
    mediaJobIds: string[];
    historyActionId?: string;
  };

  type MediaCatalogExportResult = {
    filePath: string;
    counts: {
      videoAssets: number;
      moveVideoLinks: number;
      derivedClips: number;
    };
  };

  type MediaCatalogDiagnosticFinding = {
    severity: 'warning' | 'error';
    type: string;
    entityType: string;
    entityId: string;
    message: string;
    filePath?: string;
    relatedId?: string;
  };

  type MediaCatalogDiagnostics = {
    checkedAt: string;
    database: {
      pathLabel: string;
      bootstrapStatus: 'complete' | 'incomplete';
      bootstrapMetaValue: string | null;
    };
    counts: {
      videoAssets: number;
      sourceAssets: number;
      moveAssets: number;
      moveVideoLinks: number;
      derivedClips: number;
      clipsByStatus: Record<'pending' | 'rendering' | 'ready' | 'failed', number>;
    };
    latestExport: {
      filePath: string;
      exportedAt: string | null;
    } | null;
    integritySummary: {
      total: number;
      errors: number;
      warnings: number;
      byType: Record<string, number>;
    };
    findings: MediaCatalogDiagnosticFinding[];
  };

  export let data: {
    catalogDiagnostics: MediaCatalogDiagnostics;
    jobs: MediaJob[];
  };

  let catalogDiagnostics = data.catalogDiagnostics;
  let jobs = data.jobs;
  let statusMessage = '';
  let busyJobId: string | null = null;
  let backfillBusy = false;
  let repairScanBusy = false;
  let repairRunBusy = false;
  let exportBusy = false;
  let diagnosticsBusy = false;
  let repairResult: MediaRepairResult | null = null;
  let exportResult: MediaCatalogExportResult | null = null;
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

  const repairSummaryLabels: Array<[keyof MediaRepairSummary, string]> = [
    ['missingVariantPathsPruned', 'Missing variants pruned'],
    ['legacyMoveAssetsAdded', 'Legacy assets added'],
    ['legacyLinksAdded', 'Legacy links added'],
    ['staleGeneratedAssetsRemoved', 'Stale assets removed'],
    ['staleGeneratedFilesTrashed', 'Generated files trashed'],
    ['orphanDraftMoveIdsRelinked', 'Draft ids relinked'],
    ['generatedRenameActions', 'Generated renames'],
    ['generatedVariantLinksRemoved', 'Variant links removed']
  ];

  function repairTotal(result: MediaRepairResult | null) {
    if (!result) {
      return 0;
    }
    return Object.values(result.summary).reduce((total, count) => total + count, 0);
  }

  function diagnosticsTypeSummary(diagnostics: MediaCatalogDiagnostics) {
    return Object.entries(diagnostics.integritySummary.byType)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([type, count]) => `${type}: ${count}`)
      .join(', ');
  }

  async function postMediaJobAction(action: string) {
    const response = await fetch('/api/media/jobs', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ action })
    });
    const payload = await response.json();
    if (!response.ok) {
      throw new Error(payload.error ?? 'Could not complete media job action.');
    }
    return payload;
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
      const payload = await postMediaJobAction('source.hash.backfill');
      statusMessage = `Queued ${payload.queued ?? 0} source hash job${payload.queued === 1 ? '' : 's'}.`;
      await refreshJobs();
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not queue source hash backfill.';
    } finally {
      backfillBusy = false;
    }
  }

  async function scanCatalogRepairs() {
    repairScanBusy = true;
    statusMessage = '';
    try {
      const payload = await postMediaJobAction('catalog.repair.scan');
      repairResult = payload.repair;
      statusMessage = repairResult?.changed
        ? `Catalog repair scan found ${repairTotal(repairResult)} planned change${repairTotal(repairResult) === 1 ? '' : 's'}.`
        : 'Catalog repair scan found no changes.';
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not scan media catalog repairs.';
    } finally {
      repairScanBusy = false;
    }
  }

  async function runCatalogRepairs() {
    repairRunBusy = true;
    statusMessage = '';
    try {
      const payload = await postMediaJobAction('catalog.repair.run');
      repairResult = payload.repair;
      statusMessage = repairResult?.changed
        ? `Catalog repair applied ${repairTotal(repairResult)} change${repairTotal(repairResult) === 1 ? '' : 's'}.`
        : 'Catalog repair found no changes to apply.';
      await refreshJobs();
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not run media catalog repairs.';
    } finally {
      repairRunBusy = false;
    }
  }

  async function refreshCatalogDiagnostics() {
    diagnosticsBusy = true;
    statusMessage = '';
    try {
      const payload = await postMediaJobAction('catalog.diagnostics.scan');
      catalogDiagnostics = payload.diagnostics;
      statusMessage = catalogDiagnostics.integritySummary.total === 0
        ? 'Catalog diagnostics found no integrity findings.'
        : `Catalog diagnostics found ${catalogDiagnostics.integritySummary.total} finding${catalogDiagnostics.integritySummary.total === 1 ? '' : 's'}.`;
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not refresh catalog diagnostics.';
    } finally {
      diagnosticsBusy = false;
    }
  }

  async function exportCatalogSnapshot() {
    exportBusy = true;
    statusMessage = '';
    try {
      const payload = await postMediaJobAction('catalog.export.json');
      exportResult = payload.export;
      statusMessage = `Exported media catalog snapshot to ${exportResult?.filePath ?? 'JSON'}.`;
    } catch (error) {
      statusMessage = error instanceof Error ? error.message : 'Could not export media catalog snapshot.';
    } finally {
      exportBusy = false;
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
    <div class="settings-header">
      <h3>Catalog status</h3>
      <span class="settings-header-actions">
        <button type="button" disabled={diagnosticsBusy} on:click={refreshCatalogDiagnostics}>
          {diagnosticsBusy ? 'Refreshing' : 'Refresh diagnostics'}
        </button>
      </span>
    </div>
    <div class="repair-summary">
      <strong>SQLite catalog</strong>
      <span>Database: {catalogDiagnostics.database.pathLabel}</span>
      <span>Bootstrap: {catalogDiagnostics.database.bootstrapStatus}</span>
      <span>Checked: {formatDate(catalogDiagnostics.checkedAt)}</span>
      <span>Video assets: {catalogDiagnostics.counts.videoAssets}</span>
      <span>Sources: {catalogDiagnostics.counts.sourceAssets}</span>
      <span>Move assets: {catalogDiagnostics.counts.moveAssets}</span>
      <span>Move links: {catalogDiagnostics.counts.moveVideoLinks}</span>
      <span>Derived clips: {catalogDiagnostics.counts.derivedClips}</span>
      <span>
        Clip status:
        pending {catalogDiagnostics.counts.clipsByStatus.pending},
        rendering {catalogDiagnostics.counts.clipsByStatus.rendering},
        ready {catalogDiagnostics.counts.clipsByStatus.ready},
        failed {catalogDiagnostics.counts.clipsByStatus.failed}
      </span>
      {#if catalogDiagnostics.latestExport}
        <span>
          Latest export: {catalogDiagnostics.latestExport.filePath}
          {catalogDiagnostics.latestExport.exportedAt ? ` (${formatDate(catalogDiagnostics.latestExport.exportedAt)})` : ''}
        </span>
      {:else}
        <span>Latest export: None</span>
      {/if}
      <span>
        Findings: {catalogDiagnostics.integritySummary.total}
        ({catalogDiagnostics.integritySummary.errors} errors, {catalogDiagnostics.integritySummary.warnings} warnings)
      </span>
      {#if diagnosticsTypeSummary(catalogDiagnostics)}
        <span>{diagnosticsTypeSummary(catalogDiagnostics)}</span>
      {/if}
    </div>

    {#if catalogDiagnostics.findings.length === 0}
      <p class="muted settings-status">No catalog integrity findings.</p>
    {:else}
      <div class="media-jobs-table-wrap catalog-findings-wrap">
        <table class="media-file-actions-table catalog-findings-table">
          <thead>
            <tr>
              <th>Severity</th>
              <th>Type</th>
              <th>Entity</th>
              <th>Path</th>
              <th>Message</th>
            </tr>
          </thead>
          <tbody>
            {#each catalogDiagnostics.findings as finding}
              <tr>
                <td><strong>{finding.severity}</strong></td>
                <td>{finding.type}</td>
                <td>
                  <span>{finding.entityType}</span>
                  <span>{finding.entityId}</span>
                  {#if finding.relatedId}
                    <span>Related: {finding.relatedId}</span>
                  {/if}
                </td>
                <td>
                  {#if finding.filePath}
                    <span>{finding.filePath}</span>
                  {:else}
                    <span class="muted">None</span>
                  {/if}
                </td>
                <td><span>{finding.message}</span></td>
              </tr>
            {/each}
          </tbody>
        </table>
      </div>
    {/if}
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h3>Catalog repair</h3>
      <span class="settings-header-actions">
        <button type="button" disabled={repairScanBusy || repairRunBusy} on:click={scanCatalogRepairs}>
          {repairScanBusy ? 'Scanning' : 'Scan repairs'}
        </button>
        <button
          type="button"
          disabled={repairRunBusy || repairScanBusy || !repairResult?.changed}
          on:click={runCatalogRepairs}
        >
          {repairRunBusy ? 'Running' : 'Run repairs'}
        </button>
      </span>
    </div>
    <p class="muted settings-status">
      Repair scans legacy media links, generated cleanup, missing variants, and orphaned draft ids.
    </p>
    {#if repairResult}
      <div class="repair-summary">
        <strong>{repairResult.changed ? (repairResult.dryRun ? 'Changes found' : 'Changes applied') : 'No changes'}</strong>
        <span>{repairResult.dryRun ? 'Dry run' : 'Applied run'}</span>
        {#each repairSummaryLabels as [key, label]}
          <span>{label}: {repairResult.summary[key]}</span>
        {/each}
        {#if repairResult.mediaJobIds.length > 0}
          <span>Media jobs: {repairResult.mediaJobIds.join(', ')}</span>
        {/if}
        {#if repairResult.historyActionId}
          <span>History action: {repairResult.historyActionId}</span>
        {/if}
      </div>
    {/if}
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h3>Catalog export</h3>
      <span class="settings-header-actions">
        <button type="button" disabled={exportBusy} on:click={exportCatalogSnapshot}>
          {exportBusy ? 'Exporting' : 'Export JSON snapshot'}
        </button>
      </span>
    </div>
    <p class="muted settings-status">
      Export creates a point-in-time JSON backup from SQLite. It is not live catalog state.
    </p>
    {#if exportResult}
      <div class="repair-summary">
        <strong>Snapshot exported</strong>
        <span>{exportResult.filePath}</span>
        <span>Video assets: {exportResult.counts.videoAssets}</span>
        <span>Move links: {exportResult.counts.moveVideoLinks}</span>
        <span>Derived clips: {exportResult.counts.derivedClips}</span>
      </div>
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

  .repair-summary {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(13rem, 1fr));
    gap: 0.35rem 1rem;
    margin-top: 0.75rem;
  }

  .repair-summary span {
    overflow-wrap: anywhere;
    color: var(--muted);
    font-size: 0.86rem;
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

  .catalog-findings-table {
    min-width: 1040px;
  }
</style>
