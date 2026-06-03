# Shared UI Audit

Phase 0 / Infra-1 audit of existing reusable UI and local patterns.

## Existing Shared Pieces

- `frontend/src/lib/components/MovePicker.svelte`: move-specific searchable picker with chips, keyboard selection, optional posters, and floating dropdown support.
- `frontend/src/lib/components/MoveRelationshipDiagram.svelte`: relationship diagram renderer with Graphviz SVG output, pan, zoom, and fullscreen handling.
- `frontend/src/lib/badge-settings.ts`: persistent color settings for media timing, type, and environment badges.
- `frontend/src/app.css`: shared panel, topbar, dashboard, move editor, media card, timeline, segmented-control, chip, and badge styling.

## Local Patterns To Reuse

- Media timing/type/environment badges currently live as page-local markup and CSS classes.
- The media editor has the richest editor shell pattern: left media list, detail panel, compact metadata editor, segmented controls, timeline controls, and render status.
- The dashboard overview renderer is currently route-local, but its column/filter logic can be adapted for topic and family splash pages.
- Move editing already uses `MovePicker` for relationship fields and should use the same picker behavior for new structured fields.

## Missing Shared Infrastructure

- Shared content status model for legacy, modern draft, modern published, processed, unprocessed, changed unpublished, and never published states.
- Generic badge component that can render media metadata and publication/status labels consistently.
- Generic searchable picker that can support topics, families, dancers, regions, and other small reference sets.
- Shared entity picker module that presents the same wrapper surface for family, dancer, and move fields while delegating to the appropriate search engine underneath.
- Reusable editable list/grid shell for view-by-default metadata management.
- Shared context menu interaction with right-click and long-press support.
- Shared overview filter adapter for topic/family filtered overview pages.

## Shared UI Contracts

- Status language must be shared across moves and media for legacy, modern draft, modern published, processed, unprocessed, changed unpublished, and never-published states.
- Timing, type, and environment should be displayed as badges wherever those concepts are shown on move or media surfaces.
- Publication state, processing state, and legacy/modern markers should use the same badge/status language across move pages, media browsing, and clip editing.
- Searchable pickers must be the shared pattern for topics, families, dancers, regions, and move selection.
- Family, dancer, and move add/search fields should be exercised first through the picker lab test page and then adopted from the shared entity picker module rather than reimplemented locally.
- The picker lab move family picker must be single-select, with no extra input/background prompt visible after one family is selected.
- Picker lab values created on the page must behave like real picker options for later search, selection, display, and removal.
- The picker lab move picker must search the full move catalog, not a sampled subset.
- The picker lab must expose loaded option counts and only use visible suggestion limits, not hidden dataset limits, so picker functionality can be tested in the page.
- Picker lab family, dancer, and move picker variants must use a consistent text scale for field headers, helper text, input placeholders, selected chips, suggestion rows, and lab state readouts.
- Picker chips must use an icon-style remove affordance inside the selected chip button instead of visible `x` text, while keeping the chip button's accessible remove label.
- Picker lab dropdown rows must share create, empty, more-results, hover, and active styling across family, dancer, and move variants.
- Picker dropdown row secondary text is optional and should be omitted when it only restates obvious status such as an existing tag.
- Picker dropdown keyboard navigation must keep the active suggestion visible inside the dropdown scroller and must not allow arrow keys to scroll the page/window while a picker query is active.
- Picker lab mobile and narrow layouts must prevent selected chips, input text, suggestion rows, and lab state readouts from overflowing their cards.
- Picker inline chip inputs must wrap chips and remaining input space within the field instead of hiding horizontal scroll, and dropdown row text must wrap or truncate inside the row without exceeding its text column.
- Picker lab must not include a browse/search-only test card; the first visible picker must accept committed values and show them as selected pills.
- Picker lab's first dancer picker must use `Search` as the in-field prompt after selected pills are present, not `Filter another dancer`.
- Shared entity picker templates must declare picker intent through mode (`browse`, `filter`, `singleEdit`, `multiEdit`, or `strict`), create policy (`none`, `local`, `persistMetadata`, or `draftMove`), value source (`id` or `label`), and density (`default` or `compact`) before rollout to real pages.
- Shared entity picker strict mode must not expose create behavior or allow arbitrary typed values to become selections.
- Shared entity picker create policies only control create-row availability and event intent; page-level handlers remain responsible for persistence or draft creation.
- Shared entity picker templates may hide their wrapper header for existing form labels, but they must keep the same mode, create-policy, value-source, and density event contracts.
- Current search-only/add pickers that intentionally hide selected chips should not be rolled into the picker lab or migrated to `EntityPicker` until they have a clearly named shared contract distinct from edit/filter picker behavior.
- Move picker `No preview` poster placeholders must center the text within the thumbnail area.
- Editable list/grid UI should be a shared pattern for metadata and dancer management surfaces that are viewable by default with small edit affordances.
- Context menu interactions used for relationship editing must support right-click and long-press.
- Topic and family splash pages must reuse the existing overview browsing model.
- Overview-style move search must filter non-matching moves out of the rendered overview grid instead of showing separate search results while leaving the full grid visible.
- Overview-style pages and overview search must reflect published move edits and newly published moves immediately from the live move dataset instead of waiting for a manual export refresh.

## Picker Rollout Inventory

- Media library dancer filter (`frontend/src/routes/media/+page.svelte`) is ready for `EntityPicker`: `kind: searchable`, `mode: filter`, `createPolicy: none`, `valueSource: id`, `density: compact` if needed for the filter bar. It must keep multiple selected dancer chips and no create row.
- Dancer region edit (`frontend/src/routes/dancers/+page.svelte`) is ready for `EntityPicker`: `kind: searchable`, `mode: singleEdit`, `createPolicy: local`, `valueSource: label`, `density: default`. It must keep one inside chip, allow typed local region values, and avoid page-level persistence beyond the dancer save handler.
- Move topic and family edits (`frontend/src/routes/moves/create/+page.svelte`, `frontend/src/routes/moves/[slug]/+page.svelte`, `frontend/src/routes/moves/[slug]/edit/+page.svelte`) are ready for `EntityPicker`: `kind: searchable`, `mode: singleEdit`, `createPolicy: persistMetadata`, `valueSource: label`, `density: default`. They must keep one inside chip, clear the query after select/create, and leave actual metadata persistence in the page handler.
- Move position and authorship edits (`frontend/src/routes/moves/create/+page.svelte`, `frontend/src/routes/moves/[slug]/+page.svelte`, `frontend/src/routes/moves/[slug]/edit/+page.svelte`) are ready for `EntityPicker`: `kind: searchable`, `mode: singleEdit`, `createPolicy: local`, `valueSource: label`, `density: default`. They must keep the existing single-value behavior and not persist new metadata entries automatically.
- Move tag edits (`frontend/src/routes/moves/create/+page.svelte`, `frontend/src/routes/moves/[slug]/+page.svelte`, `frontend/src/routes/moves/[slug]/edit/+page.svelte`) are ready for `EntityPicker`: `kind: searchable`, `mode: multiEdit`, `createPolicy: local`, `valueSource: label`, `density: default`. They must keep multiple inside chips and local typed tag creation.
- Media edit dancer assignment (`frontend/src/routes/media/edit/[id]/+page.svelte`) is ready for `EntityPicker`: `kind: searchable`, `mode: multiEdit`, `createPolicy: none`, `valueSource: label`, `density: compact`. It must keep floating dropdown behavior, multiple selected dancer chips, and no create row.
- Media edit draft move rows (`frontend/src/routes/media/edit/[id]/+page.svelte`) are ready for `EntityPicker` only for the move-cell picker: `kind: move`, `mode: singleEdit`, `createPolicy: draftMove`, `valueSource: id`, `density: compact`. It must keep max-one move, floating dropdown behavior, draft-move create events, and keyboard scrolling containment.
- Media edit draft start/end position rows (`frontend/src/routes/media/edit/[id]/+page.svelte`) are ready for `EntityPicker`: `kind: searchable`, `mode: strict`, `createPolicy: none`, `valueSource: id`, `density: compact`. They must keep one selected known position, floating dropdown behavior, and no arbitrary typed commit.
- Relationship diagram add pickers (`frontend/src/lib/components/MoveConnectionDiagramEditor.svelte`), pinned move search (`frontend/src/routes/moves/create/+page.svelte`), dancer list search (`frontend/src/routes/dancers/+page.svelte`), and topic/family metadata list search (`frontend/src/routes/moves/create/metadata/+page.svelte`) should stay direct for now. They are search-only/add/navigation controls with `showSelected=false`, so migrating them before a separate shared search-only contract would recreate the confusing no-chip picker behavior that was removed from the lab.

## Cross-Page Contract

- If a feature appears in multiple areas, its shared interaction belongs here first and page docs should reference this contract rather than redefining a different local behavior.
- Page-specific docs may add local constraints, but they should not fork shared badge, picker, context menu, status, or overview behavior without explicit user approval.
