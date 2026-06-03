export type EntityPickerSearchOption = {
  id: string;
  label: string;
  secondary?: string | null;
  imageUrl?: string | null;
};

export type EntityPickerMoveOption = {
  id: string;
  displayId?: string | null;
  slug?: string | null;
  name?: string | null;
  posterFile?: string | null;
  thumbnailUrl?: string | null;
  level?: string | null;
  isDraft?: boolean;
};

export type EntityPickerMode = 'browse' | 'filter' | 'singleEdit' | 'multiEdit' | 'strict';
export type EntityPickerCreatePolicy = 'none' | 'local' | 'persistMetadata' | 'draftMove';
export type EntityPickerValueSource = 'id' | 'label';
export type EntityPickerDensity = 'default' | 'compact';

type EntityPickerBaseTemplate = {
  key: string;
  title: string;
  description: string;
  placeholder: string;
  addPlaceholder: string;
  ariaLabel: string;
  emptyText?: string;
  moreText?: string;
  limit?: number;
  mode?: EntityPickerMode;
  createPolicy?: EntityPickerCreatePolicy;
  valueSource?: EntityPickerValueSource;
  density?: EntityPickerDensity;
  allowCreate?: boolean;
  selectedPlacement?: 'before' | 'after' | 'inside';
  floatingDropdown?: boolean;
  showSelected?: boolean;
  maxSelected?: number | null;
};

export type EntityPickerTemplate =
  | (EntityPickerBaseTemplate & {
      kind: 'searchable';
      createLabel?: string;
    })
  | (EntityPickerBaseTemplate & {
      kind: 'move';
      createText?: string;
      showId?: boolean;
      showName?: boolean;
      showPoster?: boolean;
    });
