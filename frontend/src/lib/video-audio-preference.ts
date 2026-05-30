const VIDEO_AUDIO_PREFERENCE_STORAGE_KEY = 'salsa-encyclopedia:video-audio-preference';
const VIDEO_AUDIO_PREFERENCE_TTL_MS = 3 * 60 * 60 * 1000;

type StoredVideoAudioPreference = {
  muted: boolean;
  volume: number;
  expiresAt: number;
};

export type VideoAudioPreference = {
  muted: boolean;
  volume: number;
};

export function normalizeVideoVolume(value: number) {
  if (!Number.isFinite(value)) {
    return 1;
  }

  return Math.min(1, Math.max(0, value));
}

export function loadVideoAudioPreference(): VideoAudioPreference | null {
  if (typeof window === 'undefined') {
    return null;
  }

  const rawValue = window.localStorage.getItem(VIDEO_AUDIO_PREFERENCE_STORAGE_KEY);
  if (!rawValue) {
    return null;
  }

  try {
    const parsedValue = JSON.parse(rawValue) as Partial<StoredVideoAudioPreference>;
    if (!Number.isFinite(parsedValue.expiresAt) || Number(parsedValue.expiresAt) <= Date.now()) {
      window.localStorage.removeItem(VIDEO_AUDIO_PREFERENCE_STORAGE_KEY);
      return null;
    }

    return {
      muted: Boolean(parsedValue.muted),
      volume: normalizeVideoVolume(Number(parsedValue.volume))
    };
  } catch {
    window.localStorage.removeItem(VIDEO_AUDIO_PREFERENCE_STORAGE_KEY);
    return null;
  }
}

export function hasActiveMutedVideoPreference() {
  return loadVideoAudioPreference()?.muted ?? false;
}

export function saveVideoAudioPreference(preference: VideoAudioPreference) {
  if (typeof window === 'undefined') {
    return;
  }

  const normalizedVolume = normalizeVideoVolume(preference.volume);
  const payload: StoredVideoAudioPreference = {
    muted: Boolean(preference.muted),
    volume: preference.muted && normalizedVolume === 0 ? 1 : normalizedVolume,
    expiresAt: Date.now() + VIDEO_AUDIO_PREFERENCE_TTL_MS
  };

  window.localStorage.setItem(VIDEO_AUDIO_PREFERENCE_STORAGE_KEY, JSON.stringify(payload));
}

export function saveVideoAudioPreferenceFromElement(video: HTMLVideoElement) {
  saveVideoAudioPreference({
    muted: video.muted || video.volume === 0,
    volume: video.volume > 0 ? video.volume : 1
  });
}

export function applyVideoAudioPreference(video: HTMLVideoElement) {
  const preference = loadVideoAudioPreference() ?? { muted: false, volume: 1 };
  video.volume = preference.volume;
  video.muted = preference.muted;
  return preference;
}
