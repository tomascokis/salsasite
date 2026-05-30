import test from 'node:test';
import assert from 'node:assert/strict';

import { buildOverviewLayout, buildOverviewSearchIndex } from '../src/lib/server/overview.js';

test('existing layout rows keep placement while using live move fields', () => {
  const layout = [
    {
      column: 1,
      entries: [
        { id: null, slug: null, name: 'Basics', entryType: 'Title', group: 'Basics', level: null, type: null, layoutOrder: 1, levelOrder: null, valid: false },
        { id: 'SB000001', slug: 'SB000001', name: 'Side basic', entryType: 'Data', group: 'Basics', level: '1', type: null, layoutOrder: 2, levelOrder: 1, valid: true }
      ]
    }
  ];
  const moves = [
    {
      id: 'SB000001',
      slug: 'SIDE-BASIC',
      name: 'Side basic updated',
      topic: 'Footwork',
      group: 'Movement',
      level: '2',
      type: 'Variation',
      topicOrder: 10,
      familyOrder: 10,
      moveOrder: 10,
      valid: true
    }
  ];

  const result = buildOverviewLayout(layout, moves);
  const row = result[0].entries[1];

  assert.equal(result[0].entries[0].name, 'Basics');
  assert.equal(row.id, 'SB000001');
  assert.equal(row.slug, 'SIDE-BASIC');
  assert.equal(row.name, 'Side basic updated');
  assert.equal(row.level, '2');
  assert.equal(row.type, 'Variation');
  assert.equal(row.layoutOrder, 2);
});

test('newly published moves append to an existing topic section in sorted order', () => {
  const layout = [
    {
      column: 1,
      entries: [
        { id: null, slug: null, name: 'Basics', entryType: 'Title', group: 'Basics', level: null, type: null, layoutOrder: 1, levelOrder: null, valid: false },
        { id: 'BB000001', slug: 'BB000001', name: 'Back basic', entryType: 'Data', group: 'Basics', level: '1', type: null, layoutOrder: 2, levelOrder: 1, valid: true }
      ]
    }
  ];
  const moves = [
    {
      id: 'BB000001',
      slug: 'BB000001',
      name: 'Back basic',
      topic: 'Basics',
      group: 'Movement',
      level: '1',
      type: null,
      topicOrder: 1,
      familyOrder: 1,
      moveOrder: 1,
      valid: true
    },
    {
      id: 'AA000002',
      slug: 'AA000002',
      name: 'Added later',
      topic: 'Basics',
      group: 'Movement',
      level: '3',
      type: 'Addition',
      topicOrder: 2,
      familyOrder: 1,
      moveOrder: 4,
      valid: true
    },
    {
      id: 'AA000001',
      slug: 'AA000001',
      name: 'Added earlier',
      topic: 'Basics',
      group: 'Movement',
      level: '2',
      type: null,
      topicOrder: 1,
      familyOrder: 1,
      moveOrder: 3,
      valid: true
    }
  ];

  const result = buildOverviewLayout(layout, moves);

  assert.deepEqual(
    result[0].entries.filter((entry) => entry.entryType === 'Data').map((entry) => entry.id),
    ['BB000001', 'AA000001', 'AA000002']
  );
});

test('new topic sections are appended to the last overview column when no layout hint exists', () => {
  const layout = [
    {
      column: 1,
      entries: [
        { id: null, slug: null, name: 'Basics', entryType: 'Title', group: 'Basics', level: null, type: null, layoutOrder: 1, levelOrder: null, valid: false },
        { id: 'BB000001', slug: 'BB000001', name: 'Back basic', entryType: 'Data', group: 'Basics', level: '1', type: null, layoutOrder: 2, levelOrder: 1, valid: true }
      ]
    },
    {
      column: 2,
      entries: [
        { id: null, slug: null, name: 'Turns', entryType: 'Title', group: 'Turns', level: null, type: null, layoutOrder: 1, levelOrder: null, valid: false },
        { id: 'RT000001', slug: 'RT000001', name: 'Right turn', entryType: 'Data', group: 'Turns', level: '1', type: null, layoutOrder: 2, levelOrder: 1, valid: true }
      ]
    }
  ];
  const moves = [
    {
      id: 'BB000001',
      slug: 'BB000001',
      name: 'Back basic',
      topic: 'Basics',
      group: 'Movement',
      level: '1',
      type: null,
      topicOrder: 1,
      familyOrder: 1,
      moveOrder: 1,
      valid: true
    },
    {
      id: 'RT000001',
      slug: 'RT000001',
      name: 'Right turn',
      topic: 'Turns',
      group: 'Movement',
      level: '1',
      type: null,
      topicOrder: 1,
      familyOrder: 1,
      moveOrder: 1,
      valid: true
    },
    {
      id: 'HDL00001',
      slug: 'HDL00001',
      name: 'Hold',
      topic: 'Expression',
      group: 'Hold',
      level: '4',
      type: null,
      topicOrder: null,
      familyOrder: null,
      moveOrder: null,
      topicCol: null,
      valid: true
    }
  ];

  const result = buildOverviewLayout(layout, moves);
  const lastColumn = result[result.length - 1];

  assert.equal(lastColumn.column, 2);
  assert.deepEqual(
    lastColumn.entries.slice(-2).map((entry) => entry.name ?? entry.id),
    ['Expression', 'Hold']
  );
});

test('overview search index is built from live move fields', () => {
  const moves = [
    {
      id: 'HDL00001',
      displayId: 'HDL00001',
      slug: 'hold',
      name: 'Hold',
      topic: 'Expression',
      group: 'Hold',
      tags: 'freeze, shape',
      description: 'Pause the pattern cleanly.',
      comments: 'Used in demos.'
    }
  ];

  const [entry] = buildOverviewSearchIndex(moves);

  assert.equal(entry.id, 'HDL00001');
  assert.equal(entry.slug, 'hold');
  assert.equal(entry.title, 'Hold');
  assert.equal(entry.topic, 'Expression');
  assert.match(entry.text, /freeze, shape/);
  assert.match(entry.text, /Pause the pattern cleanly/);
  assert.match(entry.text, /Used in demos/);
});
