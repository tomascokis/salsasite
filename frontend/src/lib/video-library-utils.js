export function normalizeTags(tags) {
  const values = Array.isArray(tags) ? tags : String(tags ?? '').split(',');
  const seen = new Set();

  return values
    .map((value) => String(value).trim())
    .filter(Boolean)
    .filter((value) => {
      const key = value.toLocaleLowerCase();
      if (seen.has(key)) {
        return false;
      }
      seen.add(key);
      return true;
    });
}

export function normalizeDateString(value) {
  const text = String(value ?? '').trim();
  return /^\d{4}-\d{2}-\d{2}$/.test(text) ? text : null;
}

export function normalizeOptionalText(value) {
  const text = String(value ?? '').trim();
  return text || null;
}

export function uploadMonthKey(createdAt) {
  const date = new Date(createdAt);
  if (Number.isNaN(date.getTime())) {
    return 'Unknown';
  }

  return new Intl.DateTimeFormat('en', { month: 'long', year: 'numeric', timeZone: 'UTC' }).format(date);
}

export function sourceSuggestions(assets) {
  const classWorkshops = Array.from(
    new Set(
      assets
        .filter((asset) => asset.kind === 'source')
        .map((asset) => asset.classWorkshop)
        .filter((value) => Boolean(value))
    )
  ).sort((left, right) => left.localeCompare(right));

  const tags = Array.from(
    new Set(
      assets
        .filter((asset) => asset.kind === 'source')
        .flatMap((asset) => asset.tags ?? [])
        .filter(Boolean)
    )
  ).sort((left, right) => left.localeCompare(right));

  if (!tags.some((tag) => tag.toLocaleLowerCase() === 'low quality')) {
    tags.push('low quality');
  }

  return {
    classWorkshops,
    tags
  };
}

export function moveSuggestions(moves, query, selectedMoveIds = [], limit = 8) {
  return moveSuggestionSearch(moves, query, selectedMoveIds, limit).results;
}

export function moveSuggestionSearch(moves, query, selectedMoveIds = [], limit = 8) {
  const normalizedQuery = normalizeSearchText(query);
  if (!normalizedQuery) {
    return {
      results: [],
      total: 0
    };
  }

  const selected = new Set(selectedMoveIds.map((moveId) => String(moveId).toLocaleLowerCase()));
  const queryTokens = tokenizeSearchText(normalizedQuery);

  const matches = moves
    .map((move, index) => ({ move, index, score: scoreMoveSuggestion(move, normalizedQuery, queryTokens) }))
    .filter((entry) => {
      if (selected.has(String(entry.move.id ?? '').toLocaleLowerCase())) {
        return false;
      }

      return entry.score > 0;
    })
    .sort((left, right) => {
      if (right.score !== left.score) {
        return right.score - left.score;
      }

      const leftLabel = String(left.move.name ?? left.move.id ?? '');
      const rightLabel = String(right.move.name ?? right.move.id ?? '');
      const labelComparison = leftLabel.localeCompare(rightLabel, undefined, { sensitivity: 'base' });
      return labelComparison || left.index - right.index;
    });

  return {
    results: matches.slice(0, limit).map((entry) => entry.move),
    total: matches.length
  };
}

/**
 * @param {{ derivedClips?: DerivedClip[]; moveVideoLinks?: MoveVideoLink[] }} librarySlice
 * @param {string | null | undefined} previousMoveId
 * @param {string | null | undefined} nextMoveId
 * @param {string | null | undefined} [nextMoveDisplayId]
 */
export function rekeyClipMoveAssociations(librarySlice, previousMoveId, nextMoveId, nextMoveDisplayId = null) {
  const normalizedPreviousMoveId = String(previousMoveId ?? '').trim().toUpperCase();
  const normalizedNextMoveId = String(nextMoveId ?? '').trim().toUpperCase();
  const normalizedNextMoveDisplayId =
    String(nextMoveDisplayId ?? normalizedNextMoveId)
      .trim()
      .toUpperCase() || normalizedNextMoveId;

  if (!normalizedPreviousMoveId || !normalizedNextMoveId) {
    return {
      derivedClips: [...(librarySlice?.derivedClips ?? [])],
      moveVideoLinks: [...(librarySlice?.moveVideoLinks ?? [])],
      changed: false
    };
  }

  let changed = false;
  const derivedClips = (librarySlice?.derivedClips ?? []).map((clip) => {
    if (String(clip?.moveId ?? '').trim().toUpperCase() !== normalizedPreviousMoveId) {
      return clip;
    }

    changed = true;
    return {
      ...clip,
      moveId: normalizedNextMoveId,
      moveDisplayId: normalizedNextMoveDisplayId
    };
  });

  const seenLinks = new Set();
  const moveVideoLinks = [];
  for (const link of librarySlice?.moveVideoLinks ?? []) {
    const nextLink =
      String(link?.moveId ?? '').trim().toUpperCase() === normalizedPreviousMoveId
        ? {
            ...link,
            moveId: normalizedNextMoveId
          }
        : link;

    const dedupeKey = `${String(nextLink?.moveId ?? '').trim().toUpperCase()}::${String(nextLink?.assetId ?? '')}`;
    if (seenLinks.has(dedupeKey)) {
      changed = true;
      continue;
    }

    seenLinks.add(dedupeKey);
    moveVideoLinks.push(nextLink);
  }

  return {
    derivedClips,
    moveVideoLinks,
    changed
  };
}

function scoreMoveSuggestion(move, normalizedQuery, queryTokens) {
  const name = normalizeSearchText(move.name);
  const displayId = normalizeSearchText(move.displayId ?? move.id);
  const id = normalizeSearchText(move.id);
  const slug = normalizeSearchText(move.slug);

  return Math.max(
    scoreSearchField(name, normalizedQuery, queryTokens, 100),
    scoreSearchField(displayId, normalizedQuery, queryTokens, 90),
    scoreSearchField(id, normalizedQuery, queryTokens, 70),
    scoreSearchField(slug, normalizedQuery, queryTokens, 60)
  );
}

function scoreSearchField(value, query, queryTokens, weight) {
  if (!value) return 0;
  const paddedValue = ` ${value} `;
  const paddedQuery = ` ${query} `;

  if (value === query) {
    return weight + 1000;
  }

  if (value.startsWith(`${query} `)) {
    return weight + 800 - value.length / 100;
  }

  if (paddedValue.includes(paddedQuery)) {
    return weight + 650 - value.indexOf(query) / 100;
  }

  if (value.includes(query)) {
    return weight + 450 - value.indexOf(query) / 100;
  }

  if (queryTokens.length && queryTokens.every((token) => value.includes(token))) {
    const tokenPositions = queryTokens.map((token) => value.indexOf(token)).filter((position) => position >= 0);
    const spread = Math.max(...tokenPositions) - Math.min(...tokenPositions);
    return weight + 240 - spread / 100;
  }

  return 0;
}

function normalizeSearchText(value) {
  return String(value ?? '')
    .toLocaleLowerCase()
    .replace(/[_:\/-]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

function tokenizeSearchText(value) {
  return normalizeSearchText(value).split(' ').filter(Boolean);
}
/** @typedef {import('$lib/types').DerivedClip} DerivedClip */
/** @typedef {import('$lib/types').MoveVideoLink} MoveVideoLink */
