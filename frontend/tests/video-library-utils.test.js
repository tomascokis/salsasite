import test from 'node:test';
import assert from 'node:assert/strict';

import {
  normalizeDateString,
  moveSuggestionSearch,
  moveSuggestions,
  derivePositionOptions,
  applyDefaultKeyVideoFlags,
  generatedDerivedClipFileInfo,
  normalizeOptionalText,
  normalizeTags,
  generatedDerivedClipFileMatches,
  obsoleteGeneratedClipFilePaths,
  rekeyClipMoveAssociations,
  sourceSuggestions,
  uploadMonthKey,
  visibleMoveRowKeys
} from '../src/lib/video-library-utils.js';
import { draftMoveIdFromName, generatedMoveIdStem } from '../src/lib/move-id-utils.js';

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

test('position options are derived and custom entries override duplicates', () => {
  assert.deepEqual(
    derivePositionOptions(
      [
        { positions: 'closed, open' },
        { positions: 'Open -> hammerlock' },
        { positions: 'cross body and sweetheart' }
      ],
      [{ label: 'Open' }, 'shadow']
    ),
    [
      { id: 'closed', label: 'closed', source: 'derived' },
      { id: 'cross-body', label: 'cross body', source: 'derived' },
      { id: 'hammerlock', label: 'hammerlock', source: 'derived' },
      { id: 'open', label: 'Open', source: 'custom' },
      { id: 'shadow', label: 'shadow', source: 'custom' },
      { id: 'sweetheart', label: 'sweetheart', source: 'derived' }
    ]
  );
});

test('new clips default to key videos only for the first four clips per move', () => {
  const existing = [
    { id: 'other-1', sourceAssetId: 'source-a', moveId: 'RT000001', isKeyVideo: true },
    { id: 'other-2', sourceAssetId: 'source-a', moveId: 'RT000001', isKeyVideo: false },
    { id: 'same-source-replaced', sourceAssetId: 'source-b', moveId: 'RT000001', isKeyVideo: true }
  ];

  const next = applyDefaultKeyVideoFlags(existing, [
    { id: 'new-1', sourceAssetId: 'source-b', moveId: 'RT000001' },
    { id: 'new-2', sourceAssetId: 'source-b', moveId: 'RT000001' },
    { id: 'new-3', sourceAssetId: 'source-b', moveId: 'RT000001' },
    { id: 'new-4', sourceAssetId: 'source-b', moveId: 'RT000001' },
    { id: 'manual', sourceAssetId: 'source-b', moveId: 'RT000001', isKeyVideo: false }
  ], 'source-b');

  assert.deepEqual(next.map((clip) => clip.isKeyVideo), [true, true, false, false, false]);
});

test('visible move row keys include active row plus three closest rows', () => {
  const rows = [
    { key: 'a', startMs: 0, endMs: 1000 },
    { key: 'b', startMs: 2000, endMs: 3000 },
    { key: 'c', startMs: 4000, endMs: 5000 },
    { key: 'd', startMs: 6000, endMs: 7000 },
    { key: 'e', startMs: 8000, endMs: 9000 }
  ];

  assert.deepEqual(visibleMoveRowKeys(rows, 4500, 'e'), ['b', 'c', 'd', 'e']);
});

test('visible move row keys preserve source row order after choosing nearest rows', () => {
  const rows = [
    { key: 'early', startMs: 0, endMs: 1000 },
    { key: 'active', startMs: 10000, endMs: 11000 },
    { key: 'near-before', startMs: 3000, endMs: 3500 },
    { key: 'near-current', startMs: 4000, endMs: 5000 },
    { key: 'near-after', startMs: 5500, endMs: 6000 }
  ];

  assert.deepEqual(visibleMoveRowKeys(rows, 4500, 'active'), [
    'active',
    'near-before',
    'near-current',
    'near-after'
  ]);
});

test('visible move row keys handle no active row', () => {
  const rows = [
    { key: 'a', startMs: 0, endMs: 1000 },
    { key: 'b', startMs: 2000, endMs: 3000 },
    { key: 'c', startMs: 4000, endMs: 5000 },
    { key: 'd', startMs: 6000, endMs: 7000 },
    { key: 'e', startMs: 8000, endMs: 9000 }
  ];

  assert.deepEqual(visibleMoveRowKeys(rows, 4500, null), ['b', 'c', 'd']);
});

test('visible move row keys return all rows when there are four or fewer', () => {
  const rows = [
    { key: 'a', startMs: 0, endMs: 1000 },
    { key: 'b', startMs: 2000, endMs: 3000 },
    { key: 'c', startMs: 4000, endMs: 5000 },
    { key: 'd', startMs: 6000, endMs: 7000 }
  ];

  assert.deepEqual(visibleMoveRowKeys(rows, 4500, null), ['a', 'b', 'c', 'd']);
});

test('generated derived clip files are recognized from exact generated filenames', () => {
  const clips = [
    {
      id: '7fa2dc2e-2c05-48e7-b405-a3636e307f82',
      moveDisplayId: 'SPR00001',
      sourceDisplayName: 'Fadi & Bersy'
    },
    {
      id: 'bd9405af-c465-452c-ad8b-050d3822b0aa',
      moveDisplayId: 'RT000001',
      sourceDisplayName: 'Demo Source'
    }
  ];

  assert.equal(
    generatedDerivedClipFileMatches('video-moves/SPR00001 Fadi & Bersy 7fa2dc2e.mp4', clips),
    true
  );
  assert.equal(
    generatedDerivedClipFileMatches('video-moves/SPR00001 Fadi & Bersy 7fa2dc2e draft abc123.mp4', clips),
    true
  );
  assert.equal(generatedDerivedClipFileMatches('video-moves/SPR00001 Fadi & Bersy 7fa2dc2e action.mp4', clips), true);
  assert.equal(generatedDerivedClipFileMatches('video-moves/SPR00001 Fadi & Bersy 7fa2dc2e low.mp4', clips), true);
  assert.equal(
    generatedDerivedClipFileMatches('video-moves/SPR00001 Fadi & Bersy 7fa2dc2e draft abc123 padded low.mp4', clips),
    true
  );
});

test('generated derived clip matching rejects legacy or unrelated token filenames', () => {
  const clips = [
    {
      id: '7fa2dc2e-2c05-48e7-b405-a3636e307f82',
      moveDisplayId: 'SPR00001',
      sourceDisplayName: 'Fadi & Bersy'
    }
  ];

  assert.equal(generatedDerivedClipFileMatches('video-moves/SPR00001 Spiral legacy 7fa2dc2e [on1, music].mp4', clips), false);
  assert.equal(generatedDerivedClipFileMatches('video-moves/SPR00001 Fadi & Bersy 7fa2dc2e.mov', clips), false);
  assert.equal(generatedDerivedClipFileMatches('video-sources/SPR00001 Fadi & Bersy 7fa2dc2e.mp4', clips), false);
  assert.equal(generatedDerivedClipFileMatches('video-moves/SPR00001 Other Source 7fa2dc2e.mp4', clips), false);
});

test('generated derived clip info recognizes old orphan low-res variants without a clip record', () => {
  assert.deepEqual(
    generatedDerivedClipFileInfo('video-moves/FRG00001 Alicia and Timothe at Budapest Live 2 Mambo 8c5042c5 low.mp4'),
    {
      moveDisplayId: 'FRG00001',
      sourceDisplayName: 'Alicia and Timothe at Budapest Live 2 Mambo',
      clipToken: '8c5042c5',
      draftId: null,
      variant: 'low'
    }
  );
  assert.equal(
    generatedDerivedClipFileInfo(
      'video-moves/FRG00001 Alicia and Timothe at Budapest Live 2 Mambo 8c5042c5 padded low.mp4'
    )?.variant,
    'padded-low'
  );
  assert.equal(
    generatedDerivedClipFileInfo(
      'video-moves/FRG00001 Alicia and Timothe at Budapest Live 2 Mambo 8c5042c5 action.mp4'
    )?.variant,
    'action'
  );
});

test('obsolete generated clip cleanup preserves current render outputs', () => {
  const currentLow = 'video-moves/SPR00001 Fadi & Bersy 7fa2dc2e low.mp4';
  const currentPaddedLow = 'video-moves/SPR00001 Fadi & Bersy 7fa2dc2e padded low.mp4';
  const oldDraft = 'video-moves/SPR00001 Fadi & Bersy 7fa2dc2e draft old.mp4';

  assert.deepEqual(
    obsoleteGeneratedClipFilePaths(
      [
        currentLow,
        currentPaddedLow,
        oldDraft
      ],
      [
        'video-moves/SPR00001 Fadi & Bersy 7fa2dc2e.mp4',
        currentLow,
        currentPaddedLow
      ]
    ),
    [oldDraft]
  );
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

test('move suggestion search matches the inline move editor query', () => {
  const moves = [
    { id: 'ILT00001', slug: 'ILT00001', name: 'Inline turn' },
    { id: 'ILT00020', slug: 'ILT00020', name: 'Waist tap inline turn' },
    { id: 'WTH00001', slug: 'WTH00001', name: 'Walkthrough' }
  ];

  assert.deepEqual(moveSuggestionSearch(moves, 'inline turn', [], 8), {
    results: [
      { id: 'ILT00001', slug: 'ILT00001', name: 'Inline turn' },
      { id: 'ILT00020', slug: 'ILT00020', name: 'Waist tap inline turn' }
    ],
    total: 2
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

test('draft move ids are generated from typed move names', () => {
  assert.equal(generatedMoveIdStem('Inline turn'), 'INLINETURN');
  assert.equal(generatedMoveIdStem('  left-turn / copa  '), 'LEFTTURNCOPA');
  assert.equal(draftMoveIdFromName('Inline turn', ['INLINETURN', 'INLINETURN2']), 'INLINETURN3');
});

test('publishing a draft move rekeys linked clips and deduplicates move video links', () => {
  const result = rekeyClipMoveAssociations(
    {
      derivedClips: [
        {
          id: 'clip-1',
          moveId: 'INLINETURN3',
          moveDisplayId: 'INLINETURN3'
        },
        {
          id: 'clip-2',
          moveId: 'OUTSIDE112RIGHTNEU',
          moveDisplayId: 'OUTSIDE112RIGHTNEU'
        }
      ],
      moveVideoLinks: [
        { id: 'link-1', moveId: 'INLINETURN3', assetId: 'asset-1' },
        { id: 'link-2', moveId: 'ILT00001', assetId: 'asset-1' },
        { id: 'link-3', moveId: 'INLINETURN3', assetId: 'asset-2' }
      ]
    },
    'INLINETURN3',
    'ILT00001',
    'ILT00001'
  );

  assert.equal(result.changed, true);
  assert.deepEqual(result.derivedClips, [
    {
      id: 'clip-1',
      moveId: 'ILT00001',
      moveDisplayId: 'ILT00001'
    },
    {
      id: 'clip-2',
      moveId: 'OUTSIDE112RIGHTNEU',
      moveDisplayId: 'OUTSIDE112RIGHTNEU'
    }
  ]);
  assert.deepEqual(result.moveVideoLinks, [
    { id: 'link-1', moveId: 'ILT00001', assetId: 'asset-1' },
    { id: 'link-3', moveId: 'ILT00001', assetId: 'asset-2' }
  ]);
});

test('publishing a draft move reports changed when only move video links are rekeyed', () => {
  const result = rekeyClipMoveAssociations(
    {
      derivedClips: [],
      moveVideoLinks: [
        { id: 'link-1', moveId: 'FAIRGROUND', assetId: 'asset-1' }
      ]
    },
    'FAIRGROUND',
    'FRG00001',
    'FRG00001'
  );

  assert.equal(result.changed, true);
  assert.deepEqual(result.moveVideoLinks, [
    { id: 'link-1', moveId: 'FRG00001', assetId: 'asset-1' }
  ]);
});

test('upload month grouping uses stable UTC month labels', () => {
  assert.equal(uploadMonthKey('2026-05-18T09:30:00.000Z'), 'May 2026');
  assert.equal(uploadMonthKey('2026-05-18'), 'May 2026');
  assert.equal(uploadMonthKey('not-a-date'), 'Unknown');
});
