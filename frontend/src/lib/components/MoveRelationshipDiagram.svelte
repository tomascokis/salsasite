<script lang="ts">
  import { browser } from '$app/environment';
  import { tick } from 'svelte';
  import type { RelationshipDiagram } from '$lib/types';
  import { buildRelationshipDot } from '$lib/relationship-diagram';

  export let diagram: RelationshipDiagram;

  let svgMarkup = '';
  let renderError = '';
  let loading = true;
  let fullscreenOpen = false;
  let zoom = diagram.meta.isLarge ? 1.22 : 1;
  let fullscreenZoom = diagram.meta.isLarge ? 1.3 : 1;
  let surface: HTMLDivElement | null = null;
  let fullscreenSurface: HTMLDivElement | null = null;
  let renderToken = 0;
  let lastDot = '';
  let dot = '';
  type VizInstance = {
    renderString: (input: string, options?: Record<string, string>) => string;
  };

  let vizPromise: Promise<VizInstance> | null = null;

  const minZoom = 0.7;
  const maxZoom = 2.6;

  function parseSvgDimension(value: string | null) {
    if (!value) {
      return null;
    }

    const match = value.trim().match(/^([0-9.]+)(pt|px)?$/i);
    if (!match) {
      return null;
    }

    const numeric = Number.parseFloat(match[1]);
    const unit = match[2]?.toLowerCase() ?? 'px';

    if (!Number.isFinite(numeric)) {
      return null;
    }

    if (unit === 'pt') {
      return numeric * (96 / 72);
    }

    return numeric;
  }

  function clampZoom(value: number) {
    return Math.min(maxZoom, Math.max(minZoom, Number(value.toFixed(2))));
  }

  function getVizInstance() {
    vizPromise ??= import('$lib/vendor/viz.js').then(
      (module) => module.instance() as unknown as Promise<VizInstance>
    );
    return vizPromise;
  }

  function wheelZoom(event: WheelEvent, target: 'normal' | 'fullscreen') {
    if (!event.ctrlKey && !event.metaKey) {
      return;
    }

    event.preventDefault();
    const delta = event.deltaY < 0 ? 0.12 : -0.12;
    if (target === 'fullscreen') {
      fullscreenZoom = clampZoom(fullscreenZoom + delta);
    } else {
      zoom = clampZoom(zoom + delta);
    }
  }

  function scrollDrag(node: HTMLDivElement) {
    let startX = 0;
    let startY = 0;
    let scrollLeft = 0;
    let scrollTop = 0;
    let dragging = false;

    const onPointerMove = (event: PointerEvent) => {
      if (!dragging) {
        return;
      }

      event.preventDefault();
      node.scrollLeft = scrollLeft - (event.clientX - startX);
      node.scrollTop = scrollTop - (event.clientY - startY);
    };

    const stopDragging = () => {
      dragging = false;
      node.classList.remove('dragging');
      window.removeEventListener('pointermove', onPointerMove);
      window.removeEventListener('pointerup', stopDragging);
    };

    const onPointerDown = (event: PointerEvent) => {
      if (event.button !== 0 || (event.target instanceof Element && event.target.closest('a'))) {
        return;
      }

      dragging = true;
      startX = event.clientX;
      startY = event.clientY;
      scrollLeft = node.scrollLeft;
      scrollTop = node.scrollTop;
      node.classList.add('dragging');
      window.addEventListener('pointermove', onPointerMove, { passive: false });
      window.addEventListener('pointerup', stopDragging);
    };

    node.addEventListener('pointerdown', onPointerDown);

    return {
      destroy() {
        stopDragging();
        node.removeEventListener('pointerdown', onPointerDown);
      }
    };
  }

  function sizeSvg(container: HTMLDivElement | null, currentZoom: number) {
    const svg = container?.querySelector('svg');
    if (!(svg instanceof SVGSVGElement)) {
      return;
    }

    svg.classList.add('relationship-svg');
    let croppedWidth: number | null = null;
    let croppedHeight: number | null = null;

    if (!svg.dataset.cropped) {
      try {
        const bbox = svg.getBBox();
        if (Number.isFinite(bbox.width) && Number.isFinite(bbox.height) && bbox.width > 0 && bbox.height > 0) {
          const padding = 12;
          croppedWidth = bbox.width + padding * 2;
          croppedHeight = bbox.height + padding * 2;
          svg.setAttribute(
            'viewBox',
            `${bbox.x - padding} ${bbox.y - padding} ${croppedWidth} ${croppedHeight}`
          );
          svg.dataset.cropped = 'true';
        }
      } catch {
        // Ignore SVG bbox failures and fall back to the original Graphviz dimensions.
      }
    }

    const viewBox = svg.viewBox?.baseVal;
    const attrWidth = parseSvgDimension(svg.getAttribute('width'));
    const attrHeight = parseSvgDimension(svg.getAttribute('height'));
    const baseWidth =
      croppedWidth ||
      attrWidth ||
      viewBox?.width ||
      svg.getBoundingClientRect().width ||
      480;

    const baseHeight =
      croppedHeight ||
      attrHeight ||
      viewBox?.height ||
      svg.getBoundingClientRect().height ||
      240;

    svg.style.width = `${Math.round(baseWidth * currentZoom)}px`;
    svg.style.height = `${Math.round(baseHeight * currentZoom)}px`;

    svg.querySelectorAll('a').forEach((anchor) => {
      anchor.setAttribute('target', '_self');
      anchor.classList.add('relationship-link');
    });
  }

  async function renderDiagram(dot: string) {
    if (!browser) {
      return;
    }

    const token = ++renderToken;
    loading = true;
    renderError = '';

    try {
      const viz = await getVizInstance();
      const svg = viz.renderString(dot, { format: 'svg', engine: 'dot' });

      if (token !== renderToken) {
        return;
      }

      svgMarkup = svg;
      await tick();
      sizeSvg(surface, zoom);
      sizeSvg(fullscreenSurface, fullscreenZoom);
    } catch (error) {
      if (token !== renderToken) {
        return;
      }

      svgMarkup = '';
      renderError = error instanceof Error ? error.message : 'Could not render relationship graph.';
    } finally {
      if (token === renderToken) {
        loading = false;
      }
    }
  }

  function setZoom(next: number, target: 'normal' | 'fullscreen') {
    if (target === 'fullscreen') {
      fullscreenZoom = clampZoom(next);
      sizeSvg(fullscreenSurface, fullscreenZoom);
      return;
    }

    zoom = clampZoom(next);
    sizeSvg(surface, zoom);
  }

  function openFullscreen() {
    fullscreenOpen = true;
    fullscreenZoom = diagram.meta.isLarge ? 1.3 : 1;
    tick().then(() => sizeSvg(fullscreenSurface, fullscreenZoom));
  }

  function closeFullscreen() {
    fullscreenOpen = false;
  }

  $: dot = buildRelationshipDot(diagram);

  $: if (browser && dot && dot !== lastDot) {
    lastDot = dot;
    void renderDiagram(dot);
  }

  $: if (browser && surface) {
    sizeSvg(surface, zoom);
  }

  $: if (browser && fullscreenSurface && fullscreenOpen) {
    sizeSvg(fullscreenSurface, fullscreenZoom);
  }
</script>

<svelte:window
  on:keydown={(event) => {
    if (fullscreenOpen && event.key === 'Escape') {
      closeFullscreen();
    }
  }}
/>

<div class="relationship-diagram">
  <div class="relationship-toolbar">
    <span class="relationship-summary">
      {diagram.meta.nodeCount} moves · {diagram.meta.edgeCount} links
    </span>
    <div class="relationship-toolbar-actions">
      <button type="button" on:click={() => setZoom(zoom - 0.15, 'normal')}>−</button>
      <button type="button" on:click={() => setZoom(1, 'normal')}>100%</button>
      <button type="button" on:click={() => setZoom(zoom + 0.15, 'normal')}>+</button>
      {#if diagram.meta.isLarge}
        <button type="button" on:click={openFullscreen}>Fullscreen</button>
      {/if}
    </div>
  </div>

  {#if loading}
    <p class="muted relationship-status">Rendering relationship graph…</p>
  {:else if renderError}
    <p class="muted relationship-status">Graphviz render error: {renderError}</p>
  {:else}
    <div
      class:large={diagram.meta.isLarge}
      class:small={!diagram.meta.isLarge}
      class="relationship-viewport"
      on:wheel={(event) => wheelZoom(event, 'normal')}
      use:scrollDrag
    >
      <div class="relationship-surface" bind:this={surface}>
        {@html svgMarkup}
      </div>
    </div>
  {/if}

  {#if fullscreenOpen}
    <div
      class="relationship-overlay"
      role="dialog"
      aria-modal="true"
      aria-label="Relationship graph fullscreen"
      on:click|self={closeFullscreen}
    >
      <div class="relationship-overlay-card">
        <div class="relationship-toolbar relationship-toolbar-overlay">
          <span class="relationship-summary">
            {diagram.meta.nodeCount} moves · {diagram.meta.edgeCount} links
          </span>
          <div class="relationship-toolbar-actions">
            <button type="button" on:click={() => setZoom(fullscreenZoom - 0.15, 'fullscreen')}>−</button>
            <button type="button" on:click={() => setZoom(1, 'fullscreen')}>100%</button>
            <button type="button" on:click={() => setZoom(fullscreenZoom + 0.15, 'fullscreen')}>+</button>
            <button type="button" on:click={closeFullscreen}>Close</button>
          </div>
        </div>

        <div
          class="relationship-viewport relationship-viewport-fullscreen"
          on:wheel={(event) => wheelZoom(event, 'fullscreen')}
          use:scrollDrag
        >
          <div class="relationship-surface" bind:this={fullscreenSurface}>
            {@html svgMarkup}
          </div>
        </div>
      </div>
    </div>
  {/if}
</div>
