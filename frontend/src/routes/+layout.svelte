<script lang="ts">
  import { page } from '$app/stores';
  import { onMount } from 'svelte';
  import { applyBadgeColors, loadBadgeColors } from '$lib/badge-settings';
  import '../app.css';

  const links = [
    { href: '/', label: 'Overview' },
    { href: '/progress', label: 'Progress' },
    { href: '/media', label: 'Media' },
    { href: '/dancers', label: 'Dancers' },
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

  $: currentPath = $page.url.pathname;
  $: currentLink = activeLink(currentPath);
  $: currentPath, (isMobileMenuOpen = false);
  $: activeHref = currentLink?.href;
  $: currentLabel = currentLink?.label ?? links[0].label;

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
    </div>
    <span class="topbar-version" aria-label={`Interface version ${uiVersion}`}>
      {uiVersion} {clientReady ? 'client' : 'ssr'}
    </span>
  </header>
  <main class="page">
    <slot />
  </main>
</div>
