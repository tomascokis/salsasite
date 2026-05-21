export const BADGE_COLOR_STORAGE_KEY = 'salsa-encyclopedia:media-badge-colors';

export const DEFAULT_BADGE_COLORS = {
  timingOn1: '#e5342a',
  timingOn2: '#2e65d6',
  timingOther: '#fec700',
  contentMusic: '#8a06b3',
  contentCounts: '#d58400',
  contentOther: '#fd74ff',
  environmentClass: '#00cacb',
  environmentSocial: '#005a00'
};

export type BadgeColorKey = keyof typeof DEFAULT_BADGE_COLORS;
export type BadgeColors = Record<BadgeColorKey, string>;

const HEX_COLOR = /^#[0-9a-f]{6}$/i;

export function normalizeBadgeColors(value: unknown): BadgeColors {
  const input = typeof value === 'object' && value ? (value as Partial<BadgeColors> & Record<string, unknown>) : {};
  const legacyTiming = HEX_COLOR.test(String(input.timing ?? '')) ? String(input.timing) : null;
  const legacyContent = HEX_COLOR.test(String(input.content ?? '')) ? String(input.content) : null;
  const legacyEnvironment = HEX_COLOR.test(String(input.environment ?? '')) ? String(input.environment) : null;

  function colorFor(key: BadgeColorKey, legacy: string | null) {
    const value = String(input[key] ?? '');
    return HEX_COLOR.test(value) ? value : legacy ?? DEFAULT_BADGE_COLORS[key];
  }

  return {
    timingOn1: colorFor('timingOn1', legacyTiming),
    timingOn2: colorFor('timingOn2', legacyTiming),
    timingOther: colorFor('timingOther', legacyTiming),
    contentMusic: colorFor('contentMusic', legacyContent),
    contentCounts: colorFor('contentCounts', legacyContent),
    contentOther: colorFor('contentOther', legacyContent),
    environmentClass: colorFor('environmentClass', legacyEnvironment),
    environmentSocial: colorFor('environmentSocial', legacyEnvironment)
  };
}

export function loadBadgeColors(): BadgeColors {
  if (typeof window === 'undefined') {
    return { ...DEFAULT_BADGE_COLORS };
  }

  try {
    return normalizeBadgeColors(JSON.parse(window.localStorage.getItem(BADGE_COLOR_STORAGE_KEY) ?? '{}'));
  } catch {
    return { ...DEFAULT_BADGE_COLORS };
  }
}

export function saveBadgeColors(colors: BadgeColors) {
  if (typeof window === 'undefined') {
    return;
  }

  window.localStorage.setItem(BADGE_COLOR_STORAGE_KEY, JSON.stringify(normalizeBadgeColors(colors)));
}

export function applyBadgeColors(colors: BadgeColors) {
  if (typeof document === 'undefined') {
    return;
  }

  const normalized = normalizeBadgeColors(colors);
  document.documentElement.style.setProperty('--media-badge-timing-on1', normalized.timingOn1);
  document.documentElement.style.setProperty('--media-badge-timing-on2', normalized.timingOn2);
  document.documentElement.style.setProperty('--media-badge-timing-other', normalized.timingOther);
  document.documentElement.style.setProperty('--media-badge-content-music', normalized.contentMusic);
  document.documentElement.style.setProperty('--media-badge-content-counts', normalized.contentCounts);
  document.documentElement.style.setProperty('--media-badge-content-other', normalized.contentOther);
  document.documentElement.style.setProperty('--media-badge-environment-class', normalized.environmentClass);
  document.documentElement.style.setProperty('--media-badge-environment-social', normalized.environmentSocial);
}
