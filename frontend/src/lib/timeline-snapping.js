export function isMoveBoundaryTarget(target) {
  return target === 'moveStart' || target === 'moveEnd';
}

export function snapMoveBoundaryForDrag({
  target,
  valueMs,
  previousMs,
  snapConsumed,
  boundaries,
  toleranceMs = 0
}) {
  if (snapConsumed || previousMs === null || !isMoveBoundaryTarget(target)) {
    return {
      valueMs,
      previousMs: valueMs,
      snapConsumed
    };
  }

  const direction = Math.sign(valueMs - previousMs);
  if (!direction) {
    return {
      valueMs,
      previousMs,
      snapConsumed
    };
  }

  const orderedBoundaries = [...new Set(boundaries.map((value) => Math.round(value)))].sort(
    (left, right) => left - right
  );
  const snapped =
    direction > 0
      ? orderedBoundaries.find((boundary) => boundary > previousMs && boundary <= valueMs + toleranceMs)
      : [...orderedBoundaries].reverse().find((boundary) => boundary < previousMs && boundary >= valueMs - toleranceMs);

  if (snapped === undefined) {
    return {
      valueMs,
      previousMs: valueMs,
      snapConsumed
    };
  }

  return {
    valueMs: snapped,
    previousMs: snapped,
    snapConsumed: true
  };
}
