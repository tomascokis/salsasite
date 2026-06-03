<script lang="ts">
  import EntityPicker from '$lib/components/EntityPicker.svelte';
  import type { EntityPickerTemplate } from '$lib/components/entity-picker';
  import type { DancerProfile, MetadataEntry } from '$lib/types';

  type MoveOption = {
    id: string;
    displayId?: string | null;
    slug?: string | null;
    name?: string | null;
    level?: string | null;
    posterFile?: string | null;
  };

  type PickerState = {
    query: string;
    selectedIds: string[];
    createdValues: string[];
    lastAction: string;
  };

  type SearchOption = {
    id: string;
    label: string;
    secondary?: string | null;
  };

  export let data: {
    families: MetadataEntry[];
    dancers: DancerProfile[];
    moves: MoveOption[];
  };

  const templates: EntityPickerTemplate[] = [
    {
      key: 'filter',
      kind: 'searchable',
      mode: 'filter',
      createPolicy: 'none',
      valueSource: 'id',
      density: 'default',
      title: 'Filter dancers',
      description: 'Multi-select filter picker with creation disabled.',
      placeholder: 'Search dancers',
      addPlaceholder: 'Search',
      ariaLabel: 'Filter dancers',
      limit: 24
    },
    {
      key: 'family',
      kind: 'searchable',
      mode: 'singleEdit',
      createPolicy: 'persistMetadata',
      valueSource: 'label',
      density: 'default',
      title: 'Move family',
      description: 'Single-value editor that emits labels and can persist metadata.',
      placeholder: 'Search families',
      addPlaceholder: 'Change family',
      ariaLabel: 'Search families',
      createLabel: 'Use family',
      limit: 24
    },
    {
      key: 'tags',
      kind: 'searchable',
      mode: 'multiEdit',
      createPolicy: 'local',
      valueSource: 'label',
      density: 'default',
      title: 'Move tags',
      description: 'Multi-value editor for local freeform entries.',
      placeholder: 'Add tags',
      addPlaceholder: 'Add tags',
      ariaLabel: 'Tags',
      createLabel: 'Add tag',
      limit: 24
    },
    {
      key: 'position',
      kind: 'searchable',
      mode: 'strict',
      createPolicy: 'none',
      valueSource: 'id',
      density: 'compact',
      title: 'Start position',
      description: 'Compact strict picker that only accepts known options.',
      placeholder: 'Start',
      addPlaceholder: 'Start',
      ariaLabel: 'Start position',
      limit: 24
    },
    {
      key: 'draft-move',
      kind: 'move',
      mode: 'singleEdit',
      createPolicy: 'draftMove',
      valueSource: 'id',
      density: 'compact',
      title: 'Draft move',
      description: 'Compact move picker that can create a draft move.',
      placeholder: 'Search moves by id or name',
      addPlaceholder: 'Add move',
      ariaLabel: 'Search moves by id or name',
      createText: 'Create draft move',
      showPoster: true,
      showId: true,
      showName: true,
      limit: 24
    }
  ];

  const familyOptions = data.families.map((entry) => ({
    id: entry.id,
    label: entry.name,
    secondary: `${entry.moveCount} moves`
  }));
  const dancerOptions = data.dancers.map((dancer) => ({
    id: dancer.id,
    label: dancer.displayName,
    secondary: [
      dancer.fullName !== dancer.displayName ? dancer.fullName : null,
      dancer.region,
      `${dancer.moves.length} moves`
    ]
      .filter(Boolean)
      .join(' · ')
  }));
  const positionOptions: SearchOption[] = [
    { id: 'open', label: 'Open position', secondary: 'Known position' },
    { id: 'closed', label: 'Closed position', secondary: 'Known position' },
    { id: 'shadow', label: 'Shadow position', secondary: 'Known position' },
    { id: 'hammerlock', label: 'Hammerlock', secondary: 'Known position' }
  ];
  const tagOptions: SearchOption[] = [
    { id: 'social', label: 'social' },
    { id: 'class', label: 'class' },
    { id: 'beginner', label: 'beginner' },
    { id: 'musicality', label: 'musicality' }
  ];

  function optionKey(value: string) {
    return value.trim().toLocaleLowerCase();
  }

  function mergeCreatedSearchOptions(baseOptions: SearchOption[], createdValues: string[]) {
    const seen = new Set(baseOptions.flatMap((option) => [optionKey(option.id), optionKey(option.label)]));
    const createdOptions = createdValues
      .filter((value) => {
        const key = optionKey(value);
        if (!key || seen.has(key)) {
          return false;
        }
        seen.add(key);
        return true;
      })
      .map((value) => ({
        id: value,
        label: value,
        secondary: 'Created on page'
      }));

    return [...createdOptions, ...baseOptions];
  }

  function mergeCreatedMoveOptions(baseOptions: MoveOption[], createdValues: string[]) {
    const seen = new Set(baseOptions.map((option) => optionKey(option.id)));
    const createdOptions = createdValues
      .filter((value) => {
        const key = optionKey(value);
        if (!key || seen.has(key)) {
          return false;
        }
        seen.add(key);
        return true;
      })
      .map((value) => ({
        id: value,
        slug: value.toLocaleLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, ''),
        name: value,
        level: null,
        posterFile: null,
        isDraft: true
      }));

    return [...createdOptions, ...baseOptions];
  }

  let pickerStates: Record<string, PickerState> = {
    filter: {
      query: '',
      selectedIds: dancerOptions[0] ? [dancerOptions[0].id] : [],
      createdValues: [],
      lastAction: 'Ready'
    },
    family: {
      query: '',
      selectedIds: familyOptions[0] ? [familyOptions[0].label] : [],
      createdValues: [],
      lastAction: 'Ready'
    },
    tags: {
      query: '',
      selectedIds: tagOptions[0] ? [tagOptions[0].label] : [],
      createdValues: [],
      lastAction: 'Ready'
    },
    position: {
      query: '',
      selectedIds: positionOptions[0] ? [positionOptions[0].id] : [],
      createdValues: [],
      lastAction: 'Ready'
    },
    'draft-move': {
      query: '',
      selectedIds: data.moves[0] ? [data.moves[0].id] : [],
      createdValues: [],
      lastAction: 'Ready'
    }
  };

  function optionsFor(template: EntityPickerTemplate, state: PickerState) {
    if (template.key === 'filter') return mergeCreatedSearchOptions(dancerOptions, state.createdValues);
    if (template.key === 'family') return mergeCreatedSearchOptions(familyOptions, state.createdValues);
    if (template.key === 'tags') return mergeCreatedSearchOptions(tagOptions, state.createdValues);
    if (template.key === 'position') return positionOptions;
    return mergeCreatedMoveOptions(data.moves, state.createdValues);
  }

  function stateFor(key: string) {
    return pickerStates[key];
  }

  function labelFor(template: EntityPickerTemplate, state: PickerState, value: string) {
    const match = optionsFor(template, state).find((option) => {
      if ('label' in option) {
        return option.id === value || option.label === value;
      }
      return option.id === value || option.displayId === value || option.name === value;
    });
    if (!match) {
      return value;
    }
    if ('label' in match) {
      return match.label;
    }

    const publicId = match.displayId ?? match.id;
    return match.name && match.name !== publicId ? `${publicId} ${match.name}` : match.name ?? publicId;
  }

  function updateState(key: string, next: Partial<PickerState>) {
    pickerStates = {
      ...pickerStates,
      [key]: {
        ...pickerStates[key],
        ...next
      }
    };
  }

  function selectValue(key: string, value: string) {
    const state = stateFor(key);
    const template = templates.find((candidate) => candidate.key === key);

    if (state.selectedIds.includes(value)) {
      return;
    }

    const singleValue = template?.mode === 'singleEdit' || template?.mode === 'strict';
    updateState(key, {
      query: '',
      selectedIds: singleValue ? [value] : [...state.selectedIds, value],
      lastAction: `Selected ${value}`
    });
  }

  function createValue(key: string, value: string) {
    const normalized = value.trim();
    if (!normalized) {
      return;
    }

    const state = stateFor(key);
    const template = templates.find((candidate) => candidate.key === key);
    const singleValue = template?.mode === 'singleEdit' || template?.mode === 'strict';
    updateState(key, {
      query: '',
      selectedIds: state.selectedIds.includes(normalized)
        ? state.selectedIds
        : singleValue
          ? [normalized]
          : [...state.selectedIds, normalized],
      createdValues: state.createdValues.includes(normalized) ? state.createdValues : [...state.createdValues, normalized],
      lastAction: `Created ${normalized}`
    });
  }

  function removeValue(key: string, value: string) {
    const state = stateFor(key);
    updateState(key, {
      selectedIds: state.selectedIds.filter((id) => id !== value),
      lastAction: `Removed ${value}`
    });
  }
</script>

<svelte:head>
  <title>Picker Lab | Salsa Encyclopedia</title>
</svelte:head>

<div class="picker-lab-page">
  <section class="picker-lab-hero panel meta-card">
    <div class="panel-header">
      <div class="media-properties-title">
        <h2>Picker Lab</h2>
        <span>Shared module test page</span>
      </div>
    </div>
    <p>
      This page exercises one wrapper module across family, dancer, and move pickers before the shared field is rolled through
      the rest of the site.
    </p>
  </section>

  <div class="picker-lab-grid">
    {#each templates as template}
      {@const state = pickerStates[template.key]}
      {@const options = optionsFor(template, state)}
      <section class="picker-lab-card panel meta-card">
        <EntityPicker
          {template}
          {options}
          selectedIds={state.selectedIds}
          query={state.query}
          onquery={(detail) => updateState(template.key, { query: detail.query })}
          onselect={(detail) => selectValue(template.key, detail.id)}
          onremove={(detail) => removeValue(template.key, detail.id)}
          oncreate={(detail) => createValue(template.key, detail.value)}
        />

        <div class="picker-lab-state">
          <div>
            <span class="picker-lab-state-label">Selected</span>
            <p>{state.selectedIds.length ? state.selectedIds.map((value) => labelFor(template, state, value)).join(', ') : 'Nothing selected'}</p>
          </div>
          <div>
            <span class="picker-lab-state-label">Created on page</span>
            <p>{state.createdValues.length ? state.createdValues.join(', ') : 'No ad hoc values yet'}</p>
          </div>
          <div>
            <span class="picker-lab-state-label">Catalog loaded</span>
            <p>{options.length} options, showing up to {template.limit ?? 8} matches per query</p>
          </div>
          <div>
            <span class="picker-lab-state-label">Last action</span>
            <p>{state.lastAction}</p>
          </div>
        </div>
      </section>
    {/each}
  </div>
</div>
