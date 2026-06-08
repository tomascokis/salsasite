/** @typedef {import('$lib/types').LayoutColumn} LayoutColumn */
/** @typedef {import('$lib/types').LayoutEntry} LayoutEntry */
/** @typedef {import('$lib/types').MoveRecord} MoveRecord */
/** @typedef {import('$lib/types').SearchIndexEntry} SearchIndexEntry */

function normalizeComparable(value) {
  return String(value ?? '')
    .toLocaleLowerCase()
    .replace(/\s+/g, ' ')
    .trim();
}

function sectionLabelForMove(move) {
  return String(move.topic ?? move.group ?? '').trim() || 'Unsorted';
}

function compareMoveValues(left, right) {
  return String(left ?? '').localeCompare(String(right ?? ''), undefined, { sensitivity: 'base' });
}

function compareNullableNumbers(left, right) {
  const normalizedLeft = left == null ? Number.POSITIVE_INFINITY : left;
  const normalizedRight = right == null ? Number.POSITIVE_INFINITY : right;
  return normalizedLeft - normalizedRight;
}

function parseLevelOrder(value) {
  const numericValue = Number.parseFloat(String(value ?? '').trim());
  return Number.isFinite(numericValue) ? numericValue : null;
}

function compareCatalogMoves(left, right) {
  return (
    compareNullableNumbers(left.topicOrder, right.topicOrder) ||
    compareNullableNumbers(left.familyOrder, right.familyOrder) ||
    compareNullableNumbers(parseLevelOrder(left.level), parseLevelOrder(right.level)) ||
    compareNullableNumbers(left.moveOrder, right.moveOrder) ||
    compareMoveValues(left.name, right.name) ||
    compareMoveValues(left.id, right.id)
  );
}

function getMoveForLayoutEntry(entry, moveById) {
  return entry.id ? moveById.get(String(entry.id).toUpperCase()) ?? null : null;
}

function compareLayoutEntriesByCatalogOrder(left, right, moveById) {
  const leftMove = getMoveForLayoutEntry(left, moveById);
  const rightMove = getMoveForLayoutEntry(right, moveById);

  if (leftMove && rightMove) {
    return compareCatalogMoves(leftMove, rightMove);
  }

  return (
    compareNullableNumbers(left.layoutOrder, right.layoutOrder) ||
    compareMoveValues(left.name, right.name) ||
    compareMoveValues(left.id, right.id)
  );
}

function cloneTitleEntry(entry) {
  return {
    id: entry.id ?? null,
    slug: entry.slug ?? null,
    name: entry.name ?? null,
    entryType: entry.entryType ?? 'Title',
    group: entry.group ?? entry.name ?? null,
    level: entry.level ?? null,
    type: entry.type ?? null,
    layoutOrder: entry.layoutOrder ?? null,
    levelOrder: entry.levelOrder ?? null,
    valid: entry.valid ?? false
  };
}

function moveToLayoutEntry(move, fallbackGroup) {
  return {
    id: move.id,
    slug: move.slug,
    name: move.name,
    entryType: 'Data',
    group: move.topic ?? fallbackGroup ?? null,
    level: move.level,
    type: move.type,
    layoutOrder: null,
    levelOrder: null,
    valid: move.valid,
    previewVideoFile: move.previewVideoFile ?? move.videoFiles?.[0] ?? null
  };
}

function createSection(label, column, key, isSynthetic = false) {
  return {
    key,
    label,
    column,
    isSynthetic,
    titleEntry: {
      id: null,
      slug: null,
      name: label,
      entryType: 'Title',
      group: label,
      level: null,
      type: null,
      layoutOrder: null,
      levelOrder: null,
      valid: false
    },
    rows: [],
    appendedMoves: []
  };
}

/**
 * @param {LayoutColumn[]} seedLayout
 * @param {MoveRecord[]} moves
 * @returns {LayoutColumn[]}
 */
export function buildOverviewLayout(seedLayout, moves) {
  const moveById = new Map(moves.map((move) => [move.id.toUpperCase(), move]));
  const seenMoveIds = new Set();
  const columns = [...seedLayout]
    .map((column) => ({
      column: column.column,
      entries: [...column.entries].sort((a, b) => (a.layoutOrder ?? 0) - (b.layoutOrder ?? 0))
    }))
    .sort((a, b) => a.column - b.column);
  const lastColumn = columns[columns.length - 1]?.column ?? 1;
  const columnStates = new Map();
  const sectionByKey = new Map();

  for (const column of columns) {
    /** @type {{ key: string; label: string; column: number; isSynthetic: boolean; titleEntry: LayoutEntry; rows: LayoutEntry[]; appendedMoves: MoveRecord[]; }[]} */
    const sections = [];
    /** @type {{ key: string; label: string; column: number; isSynthetic: boolean; titleEntry: LayoutEntry; rows: LayoutEntry[]; appendedMoves: MoveRecord[]; } | null} */
    let currentSection = null;

    for (const entry of column.entries) {
      if (entry.entryType === 'Title') {
        const label = String(entry.name ?? entry.group ?? '').trim() || 'Unsorted';
        const key = normalizeComparable(label);
        currentSection = createSection(label, column.column, key);
        currentSection.titleEntry = cloneTitleEntry(entry);
        sections.push(currentSection);
        if (key && !sectionByKey.has(key)) {
          sectionByKey.set(key, currentSection);
        }
        continue;
      }

      if (!currentSection) {
        currentSection = createSection('Unsorted', column.column, `__untitled__${column.column}`, true);
        sections.push(currentSection);
      }

      const liveMove = entry.id ? moveById.get(String(entry.id).toUpperCase()) ?? null : null;
      if (liveMove) {
        seenMoveIds.add(liveMove.id);
        currentSection.rows.push({
          ...entry,
          id: liveMove.id,
          slug: liveMove.slug,
          name: liveMove.name,
          group: liveMove.topic ?? currentSection.label,
          level: liveMove.level,
          type: liveMove.type,
          valid: liveMove.valid,
          previewVideoFile: liveMove.previewVideoFile ?? liveMove.videoFiles?.[0] ?? null
        });
        continue;
      }

      currentSection.rows.push({ ...entry });
    }

    columnStates.set(column.column, { column: column.column, sections });
  }

  const unseenMoves = moves.filter((move) => !seenMoveIds.has(move.id)).sort(compareCatalogMoves);
  for (const move of unseenMoves) {
    const label = sectionLabelForMove(move);
    const key = normalizeComparable(label);
    let section = sectionByKey.get(key) ?? null;

    if (!section) {
      let targetColumn = move.topicCol;
      if (targetColumn == null || !columnStates.has(targetColumn)) {
        targetColumn = lastColumn;
      }

      if (!columnStates.has(targetColumn)) {
        columnStates.set(targetColumn, { column: targetColumn, sections: [] });
      }

      section = createSection(label, targetColumn, key);
      columnStates.get(targetColumn).sections.push(section);
      if (key) {
        sectionByKey.set(key, section);
      }
    }

    section.appendedMoves.push(move);
  }

  return [...columnStates.values()]
    .sort((a, b) => a.column - b.column)
    .map((columnState) => {
      let layoutOrder = 0;
      /** @type {LayoutEntry[]} */
      const entries = [];

      for (const section of columnState.sections) {
        const titleEntry = section.titleEntry ? { ...section.titleEntry } : null;
        if (titleEntry) {
          layoutOrder += 1;
          titleEntry.layoutOrder = layoutOrder;
          entries.push(titleEntry);
        }

        const rowEntries = [
          ...section.rows,
          ...[...section.appendedMoves].sort(compareCatalogMoves).map((move) => moveToLayoutEntry(move, section.label))
        ].sort((left, right) => compareLayoutEntriesByCatalogOrder(left, right, moveById));

        for (const row of rowEntries) {
          layoutOrder += 1;
          entries.push({
            ...row,
            layoutOrder
          });
        }
      }

      return {
        column: columnState.column,
        entries
      };
    })
    .filter((column) => column.entries.length > 0);
}

/**
 * @param {MoveRecord[]} moves
 * @returns {SearchIndexEntry[]}
 */
export function buildOverviewSearchIndex(moves) {
  return moves.map((move) => ({
    id: move.id,
    slug: move.slug,
    title: move.name ?? null,
    topic: move.topic ?? null,
    text: [
      move.id,
      move.displayId ?? null,
      move.slug,
      move.name ?? null,
      move.topic ?? null,
      move.group ?? null,
      move.tags ?? null,
      move.description ?? null,
      move.comments ?? null
    ]
      .filter(Boolean)
      .join(' ')
  }));
}

export function deriveOverviewTitle(entry, move) {
  if (move?.topic) {
    return move.topic;
  }

  if (entry.group) {
    return entry.group;
  }

  return 'Unsorted';
}
