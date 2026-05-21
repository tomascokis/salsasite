const LARGE_DIAGRAM_THRESHOLD = 6;
const LEAF_ROW_VISIBLE_LIMIT = 10;
const SUMMARY_NODE_PREFIX = '__summary__';

function uniqueValues(values) {
  return [...new Set(values)];
}

export function parseComponentIds(value) {
  if (!value || typeof value !== 'string') {
    return [];
  }

  return uniqueValues(
    value
      .split(/[;,+]/)
      .map((entry) => entry.trim())
      .filter(Boolean)
  );
}

function buildMoveIndex(moves) {
  return new Map(moves.map((move) => [move.id, move]));
}

function buildComponentMap(moves) {
  return new Map(
    moves.map((move) => [
      move.id,
      Array.isArray(move.parentIds) ? uniqueValues(move.parentIds.filter(Boolean)) : parseComponentIds(move.components)
    ])
  );
}

function buildChildMap(moves, componentMap) {
  const childMap = new Map();

  for (const [moveId, componentIds] of componentMap.entries()) {
    for (const componentId of componentIds) {
      const existing = childMap.get(componentId) ?? [];
      existing.push(moveId);
      childMap.set(componentId, existing);
    }
  }

  for (const move of moves) {
    for (const childId of Array.isArray(move.childIds) ? move.childIds : []) {
      const existing = childMap.get(move.id) ?? [];
      existing.push(childId);
      childMap.set(move.id, uniqueValues(existing));
    }
  }

  return childMap;
}

function buildRelatedMap(moves) {
  const relatedMap = new Map();

  for (const move of moves) {
    const relatedIds = Array.isArray(move.relatedMoveIds) ? move.relatedMoveIds : [];
    for (const relatedId of relatedIds) {
      const existing = relatedMap.get(move.id) ?? [];
      existing.push(relatedId);
      relatedMap.set(move.id, uniqueValues(existing));
    }
  }

  return relatedMap;
}

function dedupeEdges(edges) {
  const seen = new Set();
  const result = [];

  for (const edge of edges) {
    const key =
      edge.type === 'related'
        ? `related:${[edge.from, edge.to].sort().join('<->')}`
        : `${edge.type ?? 'parent-child'}:${edge.from}->${edge.to}`;
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    result.push(edge);
  }

  return result;
}

function buildInboundEdgeMap(edges) {
  const inbound = new Map();

  for (const edge of edges.filter((entry) => entry.type !== 'related')) {
    const existing = inbound.get(edge.to) ?? [];
    existing.push(edge.from);
    inbound.set(edge.to, existing);
  }

  return inbound;
}

function buildOutboundEdgeMap(edges) {
  const outbound = new Map();

  for (const edge of edges.filter((entry) => entry.type !== 'related')) {
    const existing = outbound.get(edge.from) ?? [];
    existing.push(edge.to);
    outbound.set(edge.from, existing);
  }

  return outbound;
}

function collapseLargeLeafRows(edges) {
  const inbound = buildInboundEdgeMap(edges);
  const outbound = buildOutboundEdgeMap(edges);
  const removedNodes = new Set();
  const removedEdgeKeys = new Set();
  const summaryNodes = [];
  const summaryEdges = [];

  for (const [parentId, childIds] of outbound.entries()) {
    const leafChildIds = childIds.filter((childId) => {
      const outboundChildren = outbound.get(childId) ?? [];
      const inboundParents = inbound.get(childId) ?? [];

      return outboundChildren.length === 0 && inboundParents.length === 1 && inboundParents[0] === parentId;
    });

    if (leafChildIds.length <= LEAF_ROW_VISIBLE_LIMIT) {
      continue;
    }

    const collapsedLeafChildIds = leafChildIds.slice(LEAF_ROW_VISIBLE_LIMIT);

    for (const childId of collapsedLeafChildIds) {
      removedNodes.add(childId);
      removedEdgeKeys.add(`${parentId}->${childId}`);
    }

    const summaryId = `${SUMMARY_NODE_PREFIX}${parentId}`;
    summaryNodes.push({
      id: summaryId,
      label: `(and ${collapsedLeafChildIds.length} other moves...)`,
      slug: null,
      hasVideo: false,
      isCurrent: false,
      isSummary: true
    });
    summaryEdges.push({ from: parentId, to: summaryId });
  }

  return {
    edges: dedupeEdges(
      edges
        .filter(
          (edge) =>
            !removedEdgeKeys.has(`${edge.from}->${edge.to}`) &&
            !removedNodes.has(edge.from) &&
            !removedNodes.has(edge.to)
        )
        .concat(summaryEdges)
    ),
    summaryNodes,
    removedNodes
  };
}

export function buildRelationshipDiagram(moves, currentMoveId) {
  const moveIndex = buildMoveIndex(moves);
  const componentMap = buildComponentMap(moves);
  const childMap = buildChildMap(moves, componentMap);
  const relatedMap = buildRelatedMap(moves);
  const edges = [];

  const visitedComponents = new Set([currentMoveId]);
  const walkComponents = (moveId) => {
    const componentIds = componentMap.get(moveId) ?? [];
    for (const componentId of componentIds) {
      edges.push({ from: componentId, to: moveId });
      if (!visitedComponents.has(componentId)) {
        visitedComponents.add(componentId);
        walkComponents(componentId);
      }
    }
  };

  const visitedDescendants = new Set([currentMoveId]);
  const walkDescendants = (moveId) => {
    const childIds = childMap.get(moveId) ?? [];
    for (const childId of childIds) {
      edges.push({ from: moveId, to: childId });
      if (!visitedDescendants.has(childId)) {
        visitedDescendants.add(childId);
        walkDescendants(childId);
      }
    }
  };

  walkComponents(currentMoveId);
  walkDescendants(currentMoveId);

  const hierarchyNodeIds = uniqueValues([
    currentMoveId,
    ...edges.flatMap((edge) => [edge.from, edge.to])
  ]);
  for (const nodeId of hierarchyNodeIds) {
    for (const relatedId of relatedMap.get(nodeId) ?? []) {
      edges.push({ from: nodeId, to: relatedId, type: 'related' });
    }
  }

  const uniqueEdges = dedupeEdges(edges);
  const originalNodeIds = uniqueValues([
    currentMoveId,
    ...uniqueEdges.flatMap((edge) => [edge.from, edge.to])
  ]);
  const collapsed = collapseLargeLeafRows(uniqueEdges);
  const nodeIds = uniqueValues([
    currentMoveId,
    ...collapsed.edges.flatMap((edge) => [edge.from, edge.to])
  ]);

  const nodes = nodeIds.map((nodeId) => {
    const summaryNode = collapsed.summaryNodes.find((node) => node.id === nodeId);
    if (summaryNode) {
      return summaryNode;
    }

    const move = moveIndex.get(nodeId);
    const name = move?.name?.trim() || nodeId;
    const hasVideo = Boolean(move?.hasLocalVideo);

    return {
      id: nodeId,
      label: hasVideo ? `${name} ▶` : name,
      slug: move?.slug ?? null,
      hasVideo,
      isCurrent: nodeId === currentMoveId
    };
  });

  return {
    nodes,
    edges: collapsed.edges,
    meta: {
      nodeCount: originalNodeIds.length,
      edgeCount: uniqueEdges.length,
      isLarge: nodes.length > LARGE_DIAGRAM_THRESHOLD,
      hasDiagram: nodes.length > 1
    }
  };
}

function escapeDotValue(value) {
  return String(value).replace(/\\/g, '\\\\').replace(/"/g, '\\"');
}

export function buildRelationshipDot(diagram) {
  if (!diagram?.nodes?.length) {
    return 'digraph { graph [rankdir=TB]; }';
  }

  const nodeLines = diagram.nodes.map((node) => {
    const attrs = [
      `label="${escapeDotValue(node.label)}"`,
      'shape="box"',
      'style="rounded,filled"',
      'fontname="Helvetica"',
      'fontsize="13"',
      'margin="0.18,0.11"',
      'color="#bac4cd"',
      'fillcolor="#ffffff"',
      'fontcolor="#2f3943"'
    ];

    if (node.isSummary) {
      attrs.push('color="#d7dee6"', 'fillcolor="#f7f9fb"', 'fontcolor="#74808d"', 'fontsize="12"');
    }

    if (node.hasVideo && !node.isCurrent) {
      attrs.push('color="#7ea7cc"', 'fillcolor="#eef6ff"', 'penwidth="1.4"');
    }

    if (node.isCurrent) {
      attrs.push('color="#c7a94f"', 'fillcolor="#fff3c9"', 'penwidth="1.8"');
    }

    if (node.slug && !node.isSummary) {
      attrs.push(
        `URL="${escapeDotValue(`/moves/${node.slug}`)}"`,
        'target="_self"',
        `tooltip="${escapeDotValue(node.label)}"`
      );
    }

    return `"${escapeDotValue(node.id)}" [${attrs.join(', ')}];`;
  });

  const edgeLines = diagram.edges.map(
    (edge) => {
      const attrs =
        edge.type === 'related'
          ? ' [dir=none, style=dashed, constraint=false, color="#6aa1c8", penwidth="1.2"]'
          : '';
      return `"${escapeDotValue(edge.from)}" -> "${escapeDotValue(edge.to)}"${attrs};`;
    }
  );

  return [
    'digraph {',
    '  graph [rankdir=TB, nodesep=0.4, ranksep=0.56, margin="0.08"];',
    '  node [shape=box];',
    '  edge [arrowhead=normal, arrowsize="0.7", color="#9ba6b1", penwidth="1.0"];',
    '',
    '  // Nodes',
    ...nodeLines.map((line) => `  ${line}`),
    '',
    '  // Edges',
    ...edgeLines.map((line) => `  ${line}`),
    '}'
  ].join('\n');
}
