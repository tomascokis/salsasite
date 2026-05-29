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

  export let data: {
    families: MetadataEntry[];
    dancers: DancerProfile[];
    moves: MoveOption[];
  };

  const templates: EntityPickerTemplate[] = [
    {
      key: 'family',
      kind: 'searchable',
      title: 'Move family',
      description: 'Shared searchable picker with create support for family fields.',
      placeholder: 'Search families',
      addPlaceholder: 'Add family',
      ariaLabel: 'Search families',
      allowCreate: true,
      createLabel: 'Use family',
      selectedPlacement: 'inside',
      floatingDropdown: true
    },
    {
      key: 'dancer',
      kind: 'searchable',
      title: 'Dancers',
      description: 'Same wrapper, but populated with dancer profiles and richer secondary text.',
      placeholder: 'Search dancers',
      addPlaceholder: 'Add dancer',
      ariaLabel: 'Search dancers',
      allowCreate: true,
      createLabel: 'Use dancer',
      selectedPlacement: 'inside',
      floatingDropdown: true
    },
    {
      key: 'move',
      kind: 'move',
      title: 'Moves',
      description: 'Same template surface routed into the move-aware picker with posters and move scoring.',
      placeholder: 'Search moves by id or name',
      addPlaceholder: 'Add move',
      ariaLabel: 'Search moves by id or name',
      allowCreate: true,
      createText: 'Create draft move',
      selectedPlacement: 'inside',
      floatingDropdown: true,
      showPoster: true,
      showId: true,
      showName: true
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

  let pickerStates: Record<string, PickerState> = {
    family: {
      query: '',
      selectedIds: familyOptions[0] ? [familyOptions[0].id] : [],
      createdValues: [],
      lastAction: 'Ready'
    },
    dancer: {
      query: '',
      selectedIds: dancerOptions[0] ? [dancerOptions[0].id] : [],
      createdValues: [],
      lastAction: 'Ready'
    },
    move: {
      query: '',
      selectedIds: data.moves[0] ? [data.moves[0].id] : [],
      createdValues: [],
      lastAction: 'Ready'
    }
  };

  function optionsFor(template: EntityPickerTemplate) {
    if (template.key === 'family') return familyOptions;
    if (template.key === 'dancer') return dancerOptions;
    return data.moves;
  }

  function stateFor(key: string) {
    return pickerStates[key];
  }

  function labelFor(template: EntityPickerTemplate, value: string) {
    const match = optionsFor(template).find((option) => option.id === value);
    if (!match) {
      return value;
    }
    return 'label' in match ? match.label : [match.displayId ?? match.id, match.name].filter(Boolean).join(' ');
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
    if (state.selectedIds.includes(value)) {
      return;
    }

    updateState(key, {
      selectedIds: [...state.selectedIds, value],
      lastAction: `Selected ${value}`
    });
  }

  function createValue(key: string, value: string) {
    const normalized = value.trim();
    if (!normalized) {
      return;
    }

    const state = stateFor(key);
    updateState(key, {
      selectedIds: state.selectedIds.includes(normalized) ? state.selectedIds : [...state.selectedIds, normalized],
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
      {@const state = stateFor(template.key)}
      <section class="picker-lab-card panel meta-card">
        <EntityPicker
          {template}
          options={optionsFor(template)}
          selectedIds={state.selectedIds}
          query={state.query}
          on:query={(event) => updateState(template.key, { query: event.detail.query })}
          on:select={(event) => selectValue(template.key, event.detail.id)}
          on:remove={(event) => removeValue(template.key, event.detail.id)}
          on:create={(event) => createValue(template.key, event.detail.value)}
        />

        <div class="picker-lab-state">
          <div>
            <span class="picker-lab-state-label">Selected</span>
            <p>{state.selectedIds.length ? state.selectedIds.map((value) => labelFor(template, value)).join(', ') : 'Nothing selected'}</p>
          </div>
          <div>
            <span class="picker-lab-state-label">Created on page</span>
            <p>{state.createdValues.length ? state.createdValues.join(', ') : 'No ad hoc values yet'}</p>
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
