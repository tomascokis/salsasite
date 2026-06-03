import test from 'node:test';
import assert from 'node:assert/strict';

import moves from '../../data/live/bootstrap/catalog/moves.json' with { type: 'json' };
import {
  buildRelationshipDiagram,
  buildRelationshipDot,
  parseComponentIds
} from '../src/lib/relationship-diagram.js';

function diagramFor(id) {
  return buildRelationshipDiagram(moves, id);
}

test('parseComponentIds handles separators and whitespace', () => {
  assert.deepEqual(parseComponentIds('CB000001 + SHT/0001; HLP00001, RT000001'), [
    'CB000001',
    'SHT/0001',
    'HLP00001',
    'RT000001'
  ]);
});

test('single parent relationship is preserved', () => {
  const diagram = diagramFor('BB000002');

  assert.equal(diagram.meta.nodeCount, 2);
  assert.equal(diagram.meta.edgeCount, 1);
  assert.deepEqual(diagram.edges, [{ from: 'BB000001', to: 'BB000002' }]);
});

test('multi-component move keeps both direct components and ancestor chain', () => {
  const diagram = diagramFor('CB030002');
  const nodeIds = diagram.nodes.map((node) => node.id);

  assert.equal(diagram.meta.nodeCount, 4);
  assert.equal(diagram.meta.edgeCount, 3);
  assert(nodeIds.includes('CB020002'));
  assert(nodeIds.includes('CB000001'));
  assert(nodeIds.includes('SHT/0001'));
});

test('descendant traversal uses reverse component lookup', () => {
  const diagram = diagramFor('WTH00001');
  const nodeIds = diagram.nodes.map((node) => node.id);

  assert.equal(diagram.meta.hasDiagram, true);
  assert(nodeIds.includes('CST00001'));
  assert(nodeIds.includes('WTH30001'));
  assert(nodeIds.includes('ITW40001'));
});

test('mixed upward and downward traversal matches the reference cast family', () => {
  const diagram = diagramFor('CST00001');
  const nodeIds = diagram.nodes.map((node) => node.id);

  assert.deepEqual(diagram.meta, {
    nodeCount: 5,
    edgeCount: 4,
    isLarge: false,
    hasDiagram: true
  });
  assert.deepEqual(nodeIds, ['CST00001', 'WTH00001', 'CST00002', 'CST00401', 'CST30020']);
});

test('largest current family stays bounded under the original traversal rules', () => {
  const diagram = diagramFor('ILT00001');

  assert.deepEqual(diagram.meta, {
    nodeCount: 56,
    edgeCount: 55,
    isLarge: true,
    hasDiagram: true
  });
});

test('cross body lead family remains a medium graph instead of exploding', () => {
  const diagram = diagramFor('CB000001');

  assert.deepEqual(diagram.meta, {
    nodeCount: 14,
    edgeCount: 13,
    isLarge: true,
    hasDiagram: true
  });
});

test('moves with no known graph still return a stable single-node payload', () => {
  const diagram = diagramFor('ECH20031');

  assert.deepEqual(diagram.meta, {
    nodeCount: 1,
    edgeCount: 0,
    isLarge: false,
    hasDiagram: false
  });
  assert.deepEqual(diagram.nodes.map((node) => node.id), ['ECH20031']);
});

test('unknown component references are preserved as unlinked fallback nodes', () => {
  const syntheticMoves = [
    {
      id: 'ROOT0001',
      slug: 'root',
      name: 'Root move',
      components: 'MISSING/01',
      hasLocalVideo: false
    },
    {
      id: 'CHILD001',
      slug: 'child',
      name: 'Child move',
      components: 'ROOT0001',
      hasLocalVideo: true
    }
  ];

  const diagram = buildRelationshipDiagram(syntheticMoves, 'ROOT0001');
  const missing = diagram.nodes.find((node) => node.id === 'MISSING/01');
  const child = diagram.nodes.find((node) => node.id === 'CHILD001');

  assert(missing);
  assert.equal(missing.slug, null);
  assert(child?.hasVideo);
});

test('large flat leaf rows collapse into a summary node', () => {
  const syntheticMoves = [
    {
      id: 'ROOT0001',
      slug: 'root',
      name: 'Root move',
      components: null,
      hasLocalVideo: false
    },
    ...Array.from({ length: 12 }, (_, index) => ({
      id: `LEAF${String(index + 1).padStart(4, '0')}`,
      slug: `leaf-${index + 1}`,
      name: `Leaf ${index + 1}`,
      components: 'ROOT0001',
      hasLocalVideo: false
    }))
  ];

  const diagram = buildRelationshipDiagram(syntheticMoves, 'ROOT0001');
  const summaryNode = diagram.nodes.find((node) => node.isSummary);
  const nodeIds = diagram.nodes.map((node) => node.id);

  assert.equal(diagram.meta.nodeCount, 13);
  assert.equal(diagram.meta.edgeCount, 12);
  assert(summaryNode);
  assert.equal(summaryNode.label, '(and 2 other moves...)');
  assert.equal(summaryNode.slug, null);
  assert.deepEqual(nodeIds, [
    'ROOT0001',
    'LEAF0001',
    'LEAF0002',
    'LEAF0003',
    'LEAF0004',
    'LEAF0005',
    'LEAF0006',
    'LEAF0007',
    'LEAF0008',
    'LEAF0009',
    'LEAF0010',
    '__summary__ROOT0001'
  ]);
  assert.deepEqual(diagram.edges, [
    { from: 'ROOT0001', to: 'LEAF0001' },
    { from: 'ROOT0001', to: 'LEAF0002' },
    { from: 'ROOT0001', to: 'LEAF0003' },
    { from: 'ROOT0001', to: 'LEAF0004' },
    { from: 'ROOT0001', to: 'LEAF0005' },
    { from: 'ROOT0001', to: 'LEAF0006' },
    { from: 'ROOT0001', to: 'LEAF0007' },
    { from: 'ROOT0001', to: 'LEAF0008' },
    { from: 'ROOT0001', to: 'LEAF0009' },
    { from: 'ROOT0001', to: 'LEAF0010' },
    { from: 'ROOT0001', to: '__summary__ROOT0001' }
  ]);
});

test('exactly ten flat leaf children stay fully visible', () => {
  const syntheticMoves = [
    {
      id: 'ROOT0001',
      slug: 'root',
      name: 'Root move',
      components: null,
      hasLocalVideo: false
    },
    ...Array.from({ length: 10 }, (_, index) => ({
      id: `LEAF${String(index + 1).padStart(4, '0')}`,
      slug: `leaf-${index + 1}`,
      name: `Leaf ${index + 1}`,
      components: 'ROOT0001',
      hasLocalVideo: false
    }))
  ];

  const diagram = buildRelationshipDiagram(syntheticMoves, 'ROOT0001');

  assert.equal(diagram.nodes.some((node) => node.isSummary), false);
  assert.equal(diagram.meta.nodeCount, 11);
  assert.equal(diagram.edges.length, 10);
});

test('smaller flat rows are not collapsed', () => {
  const syntheticMoves = [
    {
      id: 'ROOT0001',
      slug: 'root',
      name: 'Root move',
      components: null,
      hasLocalVideo: false
    },
    ...Array.from({ length: 9 }, (_, index) => ({
      id: `LEAF${String(index + 1).padStart(4, '0')}`,
      slug: `leaf-${index + 1}`,
      name: `Leaf ${index + 1}`,
      components: 'ROOT0001',
      hasLocalVideo: false
    }))
  ];

  const diagram = buildRelationshipDiagram(syntheticMoves, 'ROOT0001');

  assert.equal(diagram.nodes.some((node) => node.isSummary), false);
  assert.equal(diagram.meta.nodeCount, 10);
  assert.equal(diagram.edges.length, 9);
});

test('DOT output keeps slash-containing ids quoted and generates move links', () => {
  const diagram = diagramFor('CB030002');
  const dot = buildRelationshipDot(diagram);

  assert.match(dot, /"SHT\/0001"/);
  assert.match(dot, /URL="\/moves\/CB030002"/);
  assert.match(dot, /URL="\/moves\/SHT_0001"/);
});

test('related moves render as dashed non-hierarchical edges', () => {
  const syntheticMoves = [
    {
      id: 'ROOT0001',
      slug: 'root',
      name: 'Root move',
      components: null,
      parentIds: [],
      childIds: [],
      relatedMoveIds: ['SIDE0001'],
      hasLocalVideo: false
    },
    {
      id: 'SIDE0001',
      slug: 'side',
      name: 'Side move',
      components: null,
      parentIds: [],
      childIds: [],
      relatedMoveIds: ['ROOT0001'],
      hasLocalVideo: false
    }
  ];

  const diagram = buildRelationshipDiagram(syntheticMoves, 'ROOT0001');
  const dot = buildRelationshipDot(diagram);

  assert.deepEqual(diagram.edges, [{ from: 'ROOT0001', to: 'SIDE0001', type: 'related' }]);
  assert.match(dot, /style=dashed/);
  assert.match(dot, /constraint=false/);
});
