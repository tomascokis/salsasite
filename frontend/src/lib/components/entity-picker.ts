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

export type EntityPickerTemplate =
  | {
      key: string;
      kind: 'searchable';
      title: string;
      description: string;
      placeholder: string;
      addPlaceholder: string;
      ariaLabel: string;
      emptyText?: string;
      moreText?: string;
      allowCreate?: boolean;
      createLabel?: string;
      selectedPlacement?: 'before' | 'after' | 'inside';
      floatingDropdown?: boolean;
      showSelected?: boolean;
      maxSelected?: number | null;
    }
  | {
      key: string;
      kind: 'move';
      title: string;
      description: string;
      placeholder: string;
      addPlaceholder: string;
      ariaLabel: string;
      emptyText?: string;
      moreText?: string;
      allowCreate?: boolean;
      createText?: string;
      selectedPlacement?: 'before' | 'after' | 'inside';
      floatingDropdown?: boolean;
      showSelected?: boolean;
      maxSelected?: number | null;
      showId?: boolean;
      showName?: boolean;
      showPoster?: boolean;
    };
