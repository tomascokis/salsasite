import test from 'node:test';
import assert from 'node:assert/strict';

import {
  clampTimelineViewport,
  msFromTimelineRatio,
  timelineZoomed,
  zoomTimelineViewport
} from '../src/lib/timeline-zoom.js';

test('wheel zoom keeps the pointer anchored in the viewport', () => {
  assert.deepEqual(
    zoomTimelineViewport({
      viewportStartMs: 0,
      viewportEndMs: 10000,
      durationMs: 10000,
      anchorMs: 2500,
      scale: 0.5
    }),
    { startMs: 1250, endMs: 6250 }
  );
});

test('zoom out clamps to full duration at the edges', () => {
  assert.deepEqual(
    zoomTimelineViewport({
      viewportStartMs: 1000,
      viewportEndMs: 5000,
      durationMs: 8000,
      anchorMs: 1200,
      scale: 4
    }),
    { startMs: 0, endMs: 8000 }
  );
});

test('zoom in preserves the minimum timeline span', () => {
  assert.deepEqual(
    zoomTimelineViewport({
      viewportStartMs: 3000,
      viewportEndMs: 5000,
      durationMs: 10000,
      anchorMs: 4000,
      scale: 0.1
    }),
    { startMs: 3000, endMs: 5000 }
  );
});

test('clamped viewport keeps requested span inside the source duration', () => {
  assert.deepEqual(
    clampTimelineViewport({
      startMs: 9000,
      endMs: 12000,
      durationMs: 10000
    }),
    { startMs: 7000, endMs: 10000 }
  );
});

test('timeline zoom state ignores full-duration viewport', () => {
  assert.equal(timelineZoomed({ startMs: 0, endMs: 10000, durationMs: 10000 }), false);
  assert.equal(timelineZoomed({ startMs: 1000, endMs: 9000, durationMs: 10000 }), true);
});

test('timeline ratio maps into the current zoom window', () => {
  assert.equal(msFromTimelineRatio({ ratio: 0.25, viewportStartMs: 2000, viewportEndMs: 6000 }), 3000);
});
