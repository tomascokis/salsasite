export type ContentStatusKey =
  | 'legacy'
  | 'modern-draft'
  | 'modern-published'
  | 'processed'
  | 'unprocessed'
  | 'changed-unpublished'
  | 'never-published';

export type ContentStatusTone = 'neutral' | 'info' | 'success' | 'warning' | 'danger';

export interface ContentStatusDefinition {
  key: ContentStatusKey;
  label: string;
  tone: ContentStatusTone;
  description: string;
  border: 'neutral' | 'orange' | 'yellow' | 'none';
}

export const CONTENT_STATUS_DEFINITIONS: Record<ContentStatusKey, ContentStatusDefinition> = {
  legacy: {
    key: 'legacy',
    label: 'Legacy',
    tone: 'neutral',
    description: 'Imported move content with direct move clips.',
    border: 'neutral'
  },
  'modern-draft': {
    key: 'modern-draft',
    label: 'Draft',
    tone: 'info',
    description: 'Modern media-linked content with unpublished editor changes.',
    border: 'orange'
  },
  'modern-published': {
    key: 'modern-published',
    label: 'Published',
    tone: 'success',
    description: 'Modern media-linked content published to move pages.',
    border: 'none'
  },
  processed: {
    key: 'processed',
    label: 'Processed',
    tone: 'success',
    description: 'Clip output has been generated or reviewed.',
    border: 'none'
  },
  unprocessed: {
    key: 'unprocessed',
    label: 'Unprocessed',
    tone: 'neutral',
    description: 'Clip output still needs processing.',
    border: 'neutral'
  },
  'changed-unpublished': {
    key: 'changed-unpublished',
    label: 'Changed',
    tone: 'warning',
    description: 'Published modern clip has changes that have not been published to moves.',
    border: 'orange'
  },
  'never-published': {
    key: 'never-published',
    label: 'Never published',
    tone: 'warning',
    description: 'Modern move clip has not been published to a move page yet.',
    border: 'yellow'
  }
};

export interface PublicationStateInput {
  isLegacy?: boolean;
  isModern?: boolean;
  publishedAt?: string | null;
  updatedAt?: string | null;
  hasDraftChanges?: boolean;
}

export function contentStatusDefinition(key: ContentStatusKey) {
  return CONTENT_STATUS_DEFINITIONS[key];
}

export function publicationStatusFor(input: PublicationStateInput): ContentStatusKey {
  if (input.isLegacy) {
    return 'legacy';
  }

  if (!input.publishedAt) {
    return 'never-published';
  }

  if (input.hasDraftChanges || isAfter(input.updatedAt, input.publishedAt)) {
    return 'changed-unpublished';
  }

  return 'modern-published';
}

export function processingStatusFor(status: string | null | undefined): ContentStatusKey {
  if (status === 'ready' || status === 'processed') {
    return 'processed';
  }

  return 'unprocessed';
}

function isAfter(left: string | null | undefined, right: string | null | undefined) {
  if (!left || !right) {
    return false;
  }

  const leftTime = Date.parse(left);
  const rightTime = Date.parse(right);
  return Number.isFinite(leftTime) && Number.isFinite(rightTime) && leftTime > rightTime;
}
