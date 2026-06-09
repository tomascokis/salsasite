<script lang="ts">
  import { enhance } from '$app/forms';
  import type { ActionData, PageData } from './$types';

  export let data: PageData;
  export let form: ActionData;
</script>

<svelte:head>
  <title>Account | Salsa Encyclopedia</title>
</svelte:head>

<div class="account-page">
  <section class="account-panel">
    <div class="account-header">
      <div>
        <h1>Account</h1>
        <p>{data.user.username} · {data.user.role}</p>
      </div>
      <a class="header-button" href="/">Overview</a>
    </div>

    <dl class="account-details">
      <div>
        <dt>Username</dt>
        <dd>{data.user.username}</dd>
      </div>
      <div>
        <dt>Role</dt>
        <dd>{data.user.role}</dd>
      </div>
      <div>
        <dt>Last login</dt>
        <dd>{data.user.lastLoginAt ? new Date(data.user.lastLoginAt).toLocaleString() : 'Not recorded'}</dd>
      </div>
    </dl>
  </section>

  <section class="account-panel">
    <div class="account-header">
      <div>
        <h2>Change Password</h2>
      </div>
    </div>

    <form method="POST" action="?/password" class="password-form" use:enhance>
      <label>
        <span>Current password</span>
        <input name="currentPassword" type="password" autocomplete="current-password" required />
      </label>
      <label>
        <span>New password</span>
        <input name="nextPassword" type="password" autocomplete="new-password" minlength="8" required />
      </label>
      <label>
        <span>Confirm new password</span>
        <input name="confirmPassword" type="password" autocomplete="new-password" minlength="8" required />
      </label>

      {#if form?.error}
        <p class="form-error">{form.error}</p>
      {:else if form?.success}
        <p class="form-success">{form.message}</p>
      {/if}

      <div class="form-actions">
        <button type="submit" class="primary-action">Update password</button>
      </div>
    </form>
  </section>
</div>

<style>
  .account-page {
    display: grid;
    gap: 1rem;
    max-width: 760px;
    margin: 0 auto;
    padding: 1.4rem 1rem;
  }

  .account-panel {
    display: grid;
    gap: 1rem;
    padding: 1rem;
    border: 1px solid #d8e0e7;
    border-radius: 8px;
    background: #ffffff;
    box-shadow: 0 10px 24px rgba(20, 34, 46, 0.05);
  }

  .account-header {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 0.75rem;
  }

  .account-header h1,
  .account-header h2,
  .account-header p {
    margin: 0;
  }

  .account-header h1,
  .account-header h2 {
    font-size: 1.35rem;
  }

  .account-header p {
    margin-top: 0.15rem;
    color: var(--ink-soft);
    font-size: 0.86rem;
  }

  .account-details {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
    gap: 0.75rem;
    margin: 0;
  }

  .account-details div {
    display: grid;
    gap: 0.15rem;
  }

  .account-details dt {
    color: #5f6d7a;
    font-size: 0.72rem;
    font-weight: 800;
    text-transform: uppercase;
  }

  .account-details dd {
    margin: 0;
    color: #27323d;
    font-size: 0.9rem;
  }

  .password-form {
    display: grid;
    gap: 0.85rem;
  }

  .password-form label {
    display: grid;
    gap: 0.3rem;
    color: #4b5563;
    font-size: 0.9rem;
  }

  .password-form input {
    box-sizing: border-box;
    width: 100%;
    border: 1px solid #cfd8e3;
    border-radius: 6px;
    padding: 0.55rem 0.65rem;
    background: #ffffff;
    color: var(--ink);
  }

  .form-error,
  .form-success {
    margin: 0;
    font-size: 0.9rem;
  }

  .form-error {
    color: #a61919;
  }

  .form-success {
    color: #23613b;
  }

  .form-actions {
    display: flex;
    justify-content: flex-end;
  }

  .primary-action {
    border: 1px solid #1f5d91;
    border-radius: 6px;
    background: var(--accent);
    color: #ffffff;
    padding: 0.5rem 0.85rem;
    cursor: pointer;
  }

  @media (max-width: 620px) {
    .account-header {
      flex-direction: column;
    }
  }
</style>
