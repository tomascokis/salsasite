export const MIN_TIMELINE_ZOOM_SPAN_MS = 2000;

export function clampTimelineViewport({ startMs, endMs, durationMs, minSpanMs = MIN_TIMELINE_ZOOM_SPAN_MS }) {
  const duration = Math.max(0, Math.round(durationMs || 0));
  if (!duration) {
    return { startMs: 0, endMs: 0 };
  }

  const minimumSpan = Math.min(duration, Math.max(1, Math.round(minSpanMs)));
  const requestedSpan = Math.max(minimumSpan, Math.round((endMs || 0) - (startMs || 0)));
  const span = Math.min(duration, requestedSpan);
  let nextStart = Math.round(startMs || 0);
  let nextEnd = nextStart + span;

  if (nextStart < 0) {
    nextEnd -= nextStart;
    nextStart = 0;
  }

  if (nextEnd > duration) {
    nextStart -= nextEnd - duration;
    nextEnd = duration;
  }

  nextStart = Math.max(0, nextStart);
  nextEnd = Math.min(duration, Math.max(nextStart + minimumSpan, nextEnd));

  if (nextEnd > duration) {
    nextEnd = duration;
    nextStart = Math.max(0, nextEnd - span);
  }

  return { startMs: nextStart, endMs: nextEnd };
}

export function timelineZoomed({ startMs, endMs, durationMs }) {
  const duration = Math.max(0, Math.round(durationMs || 0));
  const span = Math.max(0, Math.round((endMs || 0) - (startMs || 0)));
  return duration > 0 && span > 0 && span < duration - 1;
}

export function zoomTimelineViewport({
  viewportStartMs,
  viewportEndMs,
  durationMs,
  anchorMs,
  scale,
  minSpanMs = MIN_TIMELINE_ZOOM_SPAN_MS
}) {
  const duration = Math.max(0, Math.round(durationMs || 0));
  if (!duration) {
    return { startMs: 0, endMs: 0 };
  }

  const currentStart = Math.max(0, Math.round(viewportStartMs || 0));
  const currentEnd = Math.min(duration, Math.round(viewportEndMs || duration));
  const currentSpan = Math.max(1, currentEnd - currentStart || duration);
  const safeScale = Number.isFinite(scale) && scale > 0 ? scale : 1;
  const nextSpan = Math.max(Math.min(duration, minSpanMs), Math.min(duration, currentSpan * safeScale));
  const anchor = Math.max(0, Math.min(duration, Math.round(anchorMs || 0)));
  const anchorRatio = Math.max(0, Math.min(1, (anchor - currentStart) / currentSpan));
  const nextStart = anchor - nextSpan * anchorRatio;

  return clampTimelineViewport({
    startMs: nextStart,
    endMs: nextStart + nextSpan,
    durationMs: duration,
    minSpanMs
  });
}

export function msFromTimelineRatio({ ratio, viewportStartMs, viewportEndMs }) {
  const safeRatio = Math.max(0, Math.min(1, Number.isFinite(ratio) ? ratio : 0));
  const start = Math.round(viewportStartMs || 0);
  const end = Math.round(viewportEndMs || start);
  return Math.round(start + safeRatio * (end - start));
}
