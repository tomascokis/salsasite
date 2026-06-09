<script lang="ts">
  import { page } from '$app/stores';
  import { onMount } from 'svelte';
  import { applyBadgeColors, loadBadgeColors } from '$lib/badge-settings';
  import '../app.css';

  export let data: {
    user: { username: string; role: 'viewer' | 'admin' } | null;
    isAdmin: boolean;
  };

  const baseLinks = [
    { href: '/', label: 'Overview' },
    { href: '/progress', label: 'Progress' },
    { href: '/media', label: 'Media' },
    { href: '/dancers', label: 'Dancers' }
  ];
  const adminLinks = [
    { href: '/moves/create', label: 'Create' },
    { href: '/settings', label: 'Settings' }
  ];
  const uiVersion = 'v2026.05.18-25';
  let clientReady = false;
  let isMobileMenuOpen = false;

  function normalizePath(pathname: string) {
    return pathname.replace(/\/+$/, '') || '/';
  }

  function matchesRoute(href: string, pathname: string) {
    const normalizedHref = normalizePath(href);
    const normalizedPath = normalizePath(pathname);

    return normalizedHref === '/'
      ? normalizedPath === '/'
      : normalizedPath === normalizedHref || normalizedPath.startsWith(`${normalizedHref}/`);
  }

  function activeLink(pathname = $page.url.pathname) {
    return links
      .filter((link) => matchesRoute(link.href, pathname))
      .sort((a, b) => normalizePath(b.href).length - normalizePath(a.href).length)[0];
  }

  $: links = data.user ? (data.isAdmin ? [...baseLinks, ...adminLinks] : baseLinks) : [];
  $: currentPath = $page.url.pathname;
  $: currentLink = activeLink(currentPath);
  $: currentPath, (isMobileMenuOpen = false);
  $: activeHref = currentLink?.href;
  $: currentLabel = currentLink?.label ?? links[0]?.label ?? 'Menu';

  onMount(() => {
    clientReady = true;
    applyBadgeColors(loadBadgeColors());
  });
</script>

<svelte:head>
  <title>Salsa Encyclopedia</title>
</svelte:head>

<div class="app-shell">
  <header class="topbar">
    <div class="topbar-main">
      <a class="brand-block" href="/">Salsa Encyclopedia</a>
      <span class="topbar-divider" aria-hidden="true"></span>
      {#if links.length}
        <nav class="nav-links" aria-label="Primary">
          {#each links as link}
            <a
              href={link.href}
              class:active={link.href === activeHref}
              aria-current={link.href === activeHref ? 'page' : undefined}
            >
              {link.label}
            </a>
          {/each}
        </nav>
        <div class="mobile-nav">
          <button
            type="button"
            class="mobile-nav-button"
            aria-haspopup="menu"
            aria-expanded={isMobileMenuOpen}
            on:click={() => (isMobileMenuOpen = !isMobileMenuOpen)}
          >
            <span>{currentLabel}</span>
            <span aria-hidden="true">⌄</span>
          </button>
          {#if isMobileMenuOpen}
            <nav class="mobile-nav-menu" aria-label="Primary mobile">
              {#each links as link}
                <a
                  href={link.href}
                  class:active={link.href === activeHref}
                  aria-current={link.href === activeHref ? 'page' : undefined}
                  on:click={() => (isMobileMenuOpen = false)}
                >
                  {link.label}
                </a>
              {/each}
            </nav>
          {/if}
        </div>
      {/if}
    </div>
    <div class="topbar-meta">
      {#if data.user}
        <a class="account-link" href="/account">{data.user.username} · {data.user.role}</a>
        <form method="POST" action="/logout">
          <button type="submit" class="text-button">Logout</button>
        </form>
      {/if}
      <span class="topbar-version" aria-label={`Interface version ${uiVersion}`}>
        {uiVersion} {clientReady ? 'client' : 'ssr'}
      </span>
    </div>
  </header>
  <main class="page">
    <slot />
  </main>
</div>

<style>
  .topbar-meta {
    display: inline-flex;
    align-items: center;
    gap: 0.55rem;
    flex: 0 0 auto;
    color: #66717c;
    font-size: 0.78rem;
    white-space: nowrap;
  }

  .topbar-meta form {
    margin: 0;
  }

  .account-link {
    color: inherit;
  }

  .account-link:hover,
  .account-link:focus-visible {
    color: var(--accent);
    text-decoration: underline;
  }

  .text-button {
    border: 0;
    background: transparent;
    color: var(--accent);
    cursor: pointer;
    padding: 0;
  }

  .text-button:hover,
  .text-button:focus-visible {
    text-decoration: underline;
  }

  @media (max-width: 760px) {
    .topbar-meta {
      font-size: 0.72rem;
      gap: 0.35rem;
    }
  }
</style>
