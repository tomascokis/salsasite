import test from 'node:test';
import assert from 'node:assert/strict';

import {
  normalizeDateString,
  moveSuggestionSearch,
  moveSuggestions,
  normalizeOptionalText,
  normalizeTags,
  sourceSuggestions,
  uploadMonthKey
} from '../src/lib/video-library-utils.js';

test('legacy source metadata defaults normalize safely', () => {
  assert.equal(normalizeDateString(undefined), null);
  assert.equal(normalizeDateString('not-a-date'), null);
  assert.equal(normalizeOptionalText('   '), null);
  assert.deepEqual(normalizeTags(undefined), []);
});

test('record dates and tags normalize user-entered values', () => {
  assert.equal(normalizeDateString('2026-05-18'), '2026-05-18');
  assert.equal(normalizeOptionalText('  Level 3 workshop  '), 'Level 3 workshop');
  assert.deepEqual(normalizeTags('low quality, demo, Low Quality, social'), [
    'low quality',
    'demo',
    'social'
  ]);
});

test('media suggestions come from source assets and include low quality', () => {
  const suggestions = sourceSuggestions([
    {
      kind: 'source',
      classWorkshop: 'L3 workshop',
      tags: ['demo']
    },
    {
      kind: 'move',
      classWorkshop: 'Rendered output',
      tags: ['ignored']
    },
    {
      kind: 'source',
      classWorkshop: 'L2 class',
      tags: []
    }
  ]);

  assert.deepEqual(suggestions.classWorkshops, ['L2 class', 'L3 workshop']);
  assert.deepEqual(suggestions.tags, ['demo', 'low quality']);
});

test('move suggestions match ids, slugs, and names while excluding selected moves', () => {
  const moves = [
    { id: 'RT0:0002', slug: 'RT0_0002', name: 'Early right turn' },
    { id: 'ILT00001', slug: 'ILT00001', name: 'Inline turn' },
    { id: 'SCT00001', slug: 'SCT00001', name: 'Coat scoot' }
  ];

  assert.deepEqual(
    moveSuggestions(moves, 'right', []),
    [{ id: 'RT0:0002', slug: 'RT0_0002', name: 'Early right turn' }]
  );
  assert.deepEqual(
    moveSuggestions(moves, 'rt0', []),
    [{ id: 'RT0:0002', slug: 'RT0_0002', name: 'Early right turn' }]
  );
  assert.deepEqual(
    moveSuggestions(moves, 'sct', ['SCT00001']),
    []
  );
});

test('move suggestion search reports when more results exist', () => {
  const moves = [
    { id: 'RT000001', slug: 'RT000001', name: 'Right turn one' },
    { id: 'RT000002', slug: 'RT000002', name: 'Right turn two' },
    { id: 'RT000003', slug: 'RT000003', name: 'Right turn three' }
  ];

  assert.deepEqual(moveSuggestionSearch(moves, 'right', [], 2), {
    results: moves.slice(0, 2),
    total: 3
  });
});

test('move suggestion search ranks exact and phrase name matches first', () => {
  const moves = [
    { id: 'CB040003', slug: 'CB040003', name: 'CBL /left turn waist-pass' },
    { id: 'ILB30002', slug: 'ILB30002', name: '/left turnoff + inline block + headloop' },
    { id: 'LT0/0001', slug: 'LT0_0001', name: '/Left turn' },
    { id: 'LT0/3040', slug: 'LT0_3040', name: '/Left turn swan pass' },
    { id: 'LT0/0002', slug: 'LT0_0002', name: '/early left turn' }
  ];

  assert.deepEqual(moveSuggestionSearch(moves, 'left turn', [], 4).results, [
    { id: 'LT0/0001', slug: 'LT0_0001', name: '/Left turn' },
    { id: 'LT0/3040', slug: 'LT0_3040', name: '/Left turn swan pass' },
    { id: 'CB040003', slug: 'CB040003', name: 'CBL /left turn waist-pass' },
    { id: 'LT0/0002', slug: 'LT0_0002', name: '/early left turn' }
  ]);
});

test('move suggestions exclude multiple selected moves', () => {
  const moves = [
    { id: 'RT000001', slug: 'RT000001', name: 'Right turn one' },
    { id: 'RT000002', slug: 'RT000002', name: 'Right turn two' },
    { id: 'RT000003', slug: 'RT000003', name: 'Right turn three' }
  ];

  assert.deepEqual(moveSuggestions(moves, 'right', ['RT000001', 'RT000003']), [
    { id: 'RT000002', slug: 'RT000002', name: 'Right turn two' }
  ]);
});

test('upload month grouping uses stable UTC month labels', () => {
  assert.equal(uploadMonthKey('2026-05-18T09:30:00.000Z'), 'May 2026');
  assert.equal(uploadMonthKey('2026-05-18'), 'May 2026');
  assert.equal(uploadMonthKey('not-a-date'), 'Unknown');
});
