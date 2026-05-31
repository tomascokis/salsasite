import test from 'node:test';
import assert from 'node:assert/strict';

import { snapMoveBoundaryForDrag } from '../src/lib/timeline-snapping.js';

test('move boundary drag snaps when crossing the next boundary', () => {
  assert.deepEqual(
    snapMoveBoundaryForDrag({
      target: 'moveEnd',
      valueMs: 1250,
      previousMs: 900,
      snapConsumed: false,
      boundaries: [1000, 1600]
    }),
    {
      valueMs: 1000,
      previousMs: 1000,
      snapConsumed: true
    }
  );
});

test('move boundary drag snaps when crossing the previous boundary', () => {
  assert.deepEqual(
    snapMoveBoundaryForDrag({
      target: 'moveStart',
      valueMs: 750,
      previousMs: 1300,
      snapConsumed: false,
      boundaries: [1000, 1600]
    }),
    {
      valueMs: 1000,
      previousMs: 1000,
      snapConsumed: true
    }
  );
});

test('move boundary drag snaps once then allows moving away freely', () => {
  assert.deepEqual(
    snapMoveBoundaryForDrag({
      target: 'moveEnd',
      valueMs: 1400,
      previousMs: 1000,
      snapConsumed: true,
      boundaries: [1000, 1600]
    }),
    {
      valueMs: 1400,
      previousMs: 1400,
      snapConsumed: true
    }
  );
});

test('move boundary drag snaps near a boundary within screen tolerance', () => {
  assert.deepEqual(
    snapMoveBoundaryForDrag({
      target: 'moveEnd',
      valueMs: 995,
      previousMs: 900,
      snapConsumed: false,
      boundaries: [1000],
      toleranceMs: 8
    }),
    {
      valueMs: 1000,
      previousMs: 1000,
      snapConsumed: true
    }
  );
});

test('non-move timeline targets do not snap', () => {
  assert.deepEqual(
    snapMoveBoundaryForDrag({
      target: 'playhead',
      valueMs: 1250,
      previousMs: 900,
      snapConsumed: false,
      boundaries: [1000]
    }),
    {
      valueMs: 1250,
      previousMs: 1250,
      snapConsumed: false
    }
  );
});
