<script lang="ts">
  export let data: { next: string; banned?: boolean; error?: string | null };
  export let form: { username?: string; next?: string; banned?: boolean; error?: string } | null;

  $: isBanned = Boolean(form?.banned ?? data.banned);
  $: error = form?.error ?? data.error;
</script>

<svelte:head>
  <title>Login | Salsa Encyclopedia</title>
</svelte:head>

<section class="panel">
  <div class="section-heading">
    <p class="eyebrow">Private site</p>
    <h1>Login</h1>
  </div>

  {#if isBanned}
    <p class="form-error">{error ?? 'Too many login attempts. Try again later.'}</p>
  {:else}
    <form method="POST" class="editor-form">
      <input type="hidden" name="next" value={form?.next ?? data.next} />
      <label>
        Username
        <input name="username" autocomplete="username" value={form?.username ?? ''} required />
      </label>
      <label>
        Password
        <input name="password" type="password" autocomplete="current-password" required />
      </label>
      {#if error}
        <p class="form-error">{error}</p>
      {/if}
      <div class="form-actions">
        <button type="submit" class="primary-action">Login</button>
      </div>
    </form>
  {/if}
</section>

<style>
  .panel {
    max-width: 360px;
    margin: 3rem auto;
  }

  .section-heading {
    display: grid;
    gap: 0.25rem;
    margin-bottom: 1rem;
  }

  .eyebrow {
    margin: 0;
    color: var(--ink-soft);
    font-size: 0.78rem;
    text-transform: uppercase;
    letter-spacing: 0;
  }

  h1 {
    margin: 0;
    font-size: 1.55rem;
  }

  .editor-form {
    display: grid;
    gap: 0.85rem;
  }

  label {
    display: grid;
    gap: 0.3rem;
    color: #4b5563;
    font-size: 0.9rem;
  }

  input {
    box-sizing: border-box;
    width: 100%;
    border: 1px solid #cfd8e3;
    border-radius: 6px;
    padding: 0.55rem 0.65rem;
    background: #ffffff;
    color: var(--ink);
  }

  .form-error {
    margin: 0;
    color: #a61919;
    font-size: 0.9rem;
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
</style>
