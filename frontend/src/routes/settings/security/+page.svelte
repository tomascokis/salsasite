<script lang="ts">
  import type { PageData } from './$types';

  export let data: PageData;

  $: dashboard = data.dashboard;
  $: warningUserCount = dashboard.users.filter((user) => user.warningReasons.length > 0).length;
  $: failedIpWarnings = dashboard.failedAttemptsByIp.filter((entry) => entry.warning).length;
  $: failedUsernameWarnings = dashboard.failedAttemptsByUsername.filter((entry) => entry.warning).length;
  $: activeBanCount = dashboard.ipBans.filter((ban) => ban.active).length;

  function formatDate(value: string) {
    const date = new Date(value);
    return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
  }

  function warningText(reasons: string[]) {
    return reasons.length ? reasons.join(' ') : 'OK';
  }
</script>

<svelte:head>
  <title>Security | Salsa Encyclopedia</title>
</svelte:head>

<div class="settings-page security-page">
  <section class="settings-panel">
    <div class="settings-header">
      <h2>Security</h2>
      <span class="settings-header-actions">
        <a class="header-button" href="/settings">Settings</a>
        <a class="header-button" href="/settings/history">Action history</a>
      </span>
    </div>
    <div class="security-summary">
      <span class:warning={warningUserCount > 0}>{warningUserCount} user warnings</span>
      <span class:warning={failedIpWarnings > 0}>{failedIpWarnings} IP login warnings</span>
      <span class:warning={failedUsernameWarnings > 0}>{failedUsernameWarnings} username login warnings</span>
      <span class:warning={activeBanCount > 0}>{activeBanCount} active IP bans</span>
      <span>Updated {formatDate(dashboard.generatedAt)}</span>
    </div>
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h2>IP bans</h2>
    </div>
    <p class="security-note">
      Auto-bans trigger after {dashboard.thresholds.failedLoginBanThreshold} failed logins in {dashboard.thresholds.loginBanWindowHours} hour or {dashboard.thresholds.loginViewBanThresholdPerHour} login views in one hour. Bans last {dashboard.thresholds.loginBanDurationHours} hours unless an admin removes them.
    </p>
    <div class="security-table-wrap">
      <table class="security-table">
        <thead>
          <tr>
            <th>IP</th>
            <th>Status</th>
            <th>Reason</th>
            <th>Window</th>
            <th>Expires</th>
            <th>Action</th>
          </tr>
        </thead>
        <tbody>
          {#each dashboard.ipBans as ban}
            <tr class:warning-row={ban.active}>
              <td>{ban.ipAddress}</td>
              <td>{ban.active ? 'Active' : ban.unbannedAt ? `Unbanned by ${ban.unbannedBy ?? 'admin'}` : 'Expired'}</td>
              <td>{ban.reason}</td>
              <td>{formatDate(ban.windowStartedAt)} - {formatDate(ban.windowEndedAt)}</td>
              <td>{formatDate(ban.expiresAt)}</td>
              <td>
                {#if ban.active}
                  <form method="POST" action="?/unban" class="inline-form">
                    <input type="hidden" name="ipAddress" value={ban.ipAddress} />
                    <input type="hidden" name="reason" value="Manual unban from security dashboard." />
                    <button type="submit" class="header-button">Unban</button>
                  </form>
                {:else}
                  <span class="muted">None</span>
                {/if}
              </td>
            </tr>
          {:else}
            <tr>
              <td colspan="6">No IP bans recorded.</td>
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h2>User usage</h2>
    </div>
    <div class="security-table-wrap">
      <table class="security-table">
        <thead>
          <tr>
            <th>User</th>
            <th>Role</th>
            <th>Pages hour</th>
            <th>Videos hour</th>
            <th>Pages 24h</th>
            <th>Videos 24h</th>
            <th>IPs week</th>
            <th>Status</th>
          </tr>
        </thead>
        <tbody>
          {#each dashboard.users as user}
            <tr class:warning-row={user.warningReasons.length > 0}>
              <td>{user.username}</td>
              <td>{user.role}</td>
              <td>{user.pagesThisHour}</td>
              <td>{user.videosThisHour}</td>
              <td>{user.pagesLast24Hours}</td>
              <td>{user.videosLast24Hours}</td>
              <td>{user.uniqueIpsThisWeek}</td>
              <td>{warningText(user.warningReasons)}</td>
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h2>Login activity</h2>
    </div>
    <div class="security-grid">
      <div class="security-table-wrap">
        <h3>Views by IP</h3>
        <table class="security-table compact">
          <thead>
            <tr>
              <th>IP</th>
              <th>Hour</th>
              <th>24h</th>
            </tr>
          </thead>
          <tbody>
            {#each dashboard.loginViewsByIp as entry}
              <tr>
                <td>{entry.ipAddress}</td>
                <td>{entry.viewsThisHour}</td>
                <td>{entry.viewsLast24Hours}</td>
              </tr>
            {:else}
              <tr>
                <td colspan="3">No login views recorded.</td>
              </tr>
            {/each}
          </tbody>
        </table>
      </div>

      <div class="security-table-wrap">
        <h3>Failed by IP</h3>
        <table class="security-table compact">
          <thead>
            <tr>
              <th>IP</th>
              <th>Hour</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {#each dashboard.failedAttemptsByIp as entry}
              <tr class:warning-row={entry.warning}>
                <td>{entry.ipAddress}</td>
                <td>{entry.attemptsThisHour}</td>
                <td>{entry.warning ? 'Warning' : 'OK'}</td>
              </tr>
            {:else}
              <tr>
                <td colspan="3">No failed login attempts this hour.</td>
              </tr>
            {/each}
          </tbody>
        </table>
      </div>

      <div class="security-table-wrap">
        <h3>Failed by username</h3>
        <table class="security-table compact">
          <thead>
            <tr>
              <th>Username</th>
              <th>Hour</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {#each dashboard.failedAttemptsByUsername as entry}
              <tr class:warning-row={entry.warning}>
                <td>{entry.username}</td>
                <td>{entry.attemptsThisHour}</td>
                <td>{entry.warning ? 'Warning' : 'OK'}</td>
              </tr>
            {:else}
              <tr>
                <td colspan="3">No failed login attempts this hour.</td>
              </tr>
            {/each}
          </tbody>
        </table>
      </div>

      <div class="security-table-wrap">
        <h3>Successful by username</h3>
        <table class="security-table compact">
          <thead>
            <tr>
              <th>Username</th>
              <th>Hour</th>
            </tr>
          </thead>
          <tbody>
            {#each dashboard.successfulAttemptsByUsername as entry}
              <tr>
                <td>{entry.username}</td>
                <td>{entry.attemptsThisHour}</td>
              </tr>
            {:else}
              <tr>
                <td colspan="2">No successful logins this hour.</td>
              </tr>
            {/each}
          </tbody>
        </table>
      </div>
    </div>
  </section>

  <section class="settings-panel">
    <div class="settings-header">
      <h2>Recent login attempts</h2>
    </div>
    <div class="security-table-wrap">
      <table class="security-table">
        <thead>
          <tr>
            <th>When</th>
            <th>Username</th>
            <th>IP</th>
            <th>Result</th>
          </tr>
        </thead>
        <tbody>
          {#each dashboard.recentAttempts as attempt}
            <tr class:warning-row={!attempt.success}>
              <td>{formatDate(attempt.createdAt)}</td>
              <td>{attempt.username}</td>
              <td>{attempt.ipAddress}</td>
              <td>{attempt.success ? 'Success' : 'Failed'}</td>
            </tr>
          {:else}
            <tr>
              <td colspan="4">No login attempts recorded.</td>
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  </section>
</div>

<style>
  .security-page {
    max-width: 1120px;
  }

  .security-summary {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
  }

  .security-summary span {
    display: inline-flex;
    align-items: center;
    min-height: 1.55rem;
    border: 1px solid #d1d9e2;
    border-radius: 6px;
    background: #f6f8fa;
    color: #475569;
    padding: 0 0.5rem;
    font-size: 0.78rem;
    font-weight: 800;
  }

  .security-summary span.warning {
    border-color: #e9a7a7;
    background: #fff0f0;
    color: #8d2a2a;
  }

  .security-table-wrap {
    overflow-x: auto;
  }

  .security-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
    gap: 1rem;
  }

  .security-grid h3 {
    margin: 0 0 0.4rem;
    font-size: 0.95rem;
  }

  .security-note {
    margin: 0 0 0.75rem;
    color: #475569;
    font-size: 0.82rem;
    line-height: 1.35;
  }

  .inline-form {
    margin: 0;
  }

  .security-table {
    width: 100%;
    min-width: 820px;
    border-collapse: collapse;
  }

  .security-table.compact {
    min-width: 0;
  }

  .security-table th,
  .security-table td {
    padding: 0.62rem 0.65rem;
    border-bottom: 1px solid rgba(15, 23, 42, 0.12);
    text-align: left;
    vertical-align: top;
  }

  .security-table th {
    color: #5f6d7a;
    font-size: 0.72rem;
    text-transform: uppercase;
  }

  .warning-row td {
    background: #fff8f0;
  }

  @media (max-width: 720px) {
    .security-page {
      padding-left: 0.6rem;
      padding-right: 0.6rem;
    }

    .security-summary span {
      font-size: 0.72rem;
    }
  }
</style>
