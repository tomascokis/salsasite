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
- Reusable editable list/grid shell for view-by-default metadata management.
- Shared context menu interaction with right-click and long-press support.
- Shared overview filter adapter for topic/family filtered overview pages.
