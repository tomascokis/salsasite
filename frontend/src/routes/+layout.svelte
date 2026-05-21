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

  function isActive(href: string, pathname = $page.url.pathname) {
    return href === '/' ? pathname === href : pathname === href || pathname.startsWith(`${href}/`);
  }

  function activeLink(pathname = $page.url.pathname) {
    return links.find((link) => isActive(link.href, pathname)) ?? links[0];
  }

  $: currentPath = $page.url.pathname;
  $: currentLink = activeLink(currentPath);
  $: currentPath, (isMobileMenuOpen = false);

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
            class:active={isActive(link.href)}
            aria-current={isActive(link.href) ? 'page' : undefined}
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
          <span>{currentLink.label}</span>
          <span aria-hidden="true">⌄</span>
        </button>
        {#if isMobileMenuOpen}
          <nav class="mobile-nav-menu" aria-label="Primary mobile">
            {#each links as link}
              <a
                href={link.href}
                class:active={isActive(link.href)}
                aria-current={isActive(link.href) ? 'page' : undefined}
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
