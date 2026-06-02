# Salsa Site Migration Architecture

## Recommendation

Use `SvelteKit` for the rewrite.

It is the best fit for the current site because the product is not just a static content site:

- The homepage is a structured, searchable move browser.
- Each move has a detail page with metadata, diagrams, and video variants.
- The progress area has snapshot views plus an interactive editor.
- You want a frontend and backend in one deployable app, separate from R.

`Astro` is still a reasonable alternative if the site stays mostly static forever. But once the progress editor, authenticated editing, imports, and APIs become first-class features, `SvelteKit` is the cleaner default because page rendering, data loading, forms, and server endpoints all live in one framework.

## Deployment shape

Target phase 1:

- Frontend: `SvelteKit`
- Runtime: `Node` via the SvelteKit node adapter
- Container: one Docker image, still suitable for Unraid
- Database: `SQLite` on a mounted volume for phase 1
- Static assets: move videos and any images served from a mounted media path

Why SQLite first:

- Current editing scope is small and highly structured.
- It keeps deployment simple while removing the R dependency.
- You can migrate to Postgres later without changing the route model.

Use Postgres instead if you already expect concurrent multi-user editing, accounts, or external integrations from day one.

## Current site, translated to app concepts

Current R/Quarto output in `_site_reference` maps cleanly to application routes:

- `/`:
  Multi-column move browser with search and links to detail pages.
- `/moves/[slug]`:
  Single move detail page with metadata, videos, and dependency/component diagram.
- `/progress`:
  Historical progress snapshots by date.
- `/progress/editor`:
  Interactive editor with keyboard controls and CSV import/export.

## Data model

The existing site already exposes the right entities:

- `moves`:
  Core move records from `data/dt_pw.RDS`
- `layout`:
  Column and ordering data from `data/dt_pw_lay.RDS`
- `progress_entries`:
  Per-date per-move progress records from `inputs/progress.RDS`

There is now also a raw workbook reference:

- `data_reference.xlsx`:
  `595` move IDs, matching `dt_pw.RDS`, with additional source columns such as parent/addition relationships, family/blend metadata, alternative names, tracking flags, and notes

Recommendation:

- Treat `data_reference.xlsx` as the canonical move-catalogue source for the rewrite.
- Treat `dt_pw_lay.RDS` as the current canonical layout source.
- Treat `inputs/progress.RDS` as the current canonical progress source.
- Treat local files in `videomoves/` as the primary move-media source.
- Treat Dropbox links as optional legacy metadata only.

Suggested phase 1 backend tables:

- `moves`
- `move_layout_entries`
- `progress_snapshots`
- `progress_entries`

## Action History And Undo

Live editing workflows should be designed around an action history so changes can be reviewed and undone.

The near-term implementation should keep this inside the same live-mounted Docker deployment. Do not introduce a separate service just to support history.

The action history should be added as part of the SQLite persistence architecture, not as a later bolt-on after all JSON sidecar stores have been migrated. Each mutating workflow should eventually write a durable action record with:

- action type
- target entity type and ID
- timestamp
- actor or session identifier when authentication exists
- before state needed to undo the action
- after state needed to audit or redo the action
- related file operations when media files are created, renamed, rendered, published, or deleted

Undo support should be implemented incrementally. Metadata, dancers, move drafts, move edits, and managed source-video deletes are undoable. Source-video delete undo depends on the media manager trash entries remaining available under `DATA_DIR/media-trash/<job-id>/`; if required trash files are missing, undo should fail rather than restoring partial catalog state. Generated cleanup and managed media renames are durable media-manager file actions, and `/settings/media` may expose those file actions for operational visibility, but removed-clip cleanup and obsolete-render cleanup are not user-facing history undo actions yet. Render jobs can be tracked as actions, but generated files may need retry/rebuild semantics instead of a simple byte-for-byte undo.

## Phase 1 parity scope

The first migration should aim for behavioral parity, not redesign:

1. Replicate the existing information architecture and routes.
2. Preserve the dense dashboard layout on desktop.
3. Keep move detail pages close to the current visual structure.
4. Rebuild the progress viewer from structured data.
5. Rebuild the progress editor with keyboard shortcuts and CSV import/export.
6. Move search to an app-native index instead of Quarto search scraping.

## Route plan

Recommended SvelteKit routes:

- `src/routes/+layout.svelte`
- `src/routes/+page.server.ts`
- `src/routes/+page.svelte`
- `src/routes/moves/[slug]/+page.server.ts`
- `src/routes/moves/[slug]/+page.svelte`
- `src/routes/progress/+page.server.ts`
- `src/routes/progress/+page.svelte`
- `src/routes/progress/editor/+page.server.ts`
- `src/routes/progress/editor/+page.svelte`
- `src/routes/api/progress/+server.ts`
- `src/routes/api/progress/import/+server.ts`
- `src/routes/api/search/+server.ts`

## Migration approach

Do not start by scraping `_site_reference` HTML.

Use `_site_reference` as the visual and behavioral reference, but build the new app from structured exports of:

- `data_reference.xlsx`
- `data/dt_pw.RDS`
- `data/dt_pw_lay.RDS`
- `inputs/progress.RDS`

That gives you:

- stable contracts for frontend development
- easier testing
- simpler future database import
- no coupling to Quarto-specific HTML output

## Immediate next step

The repo now includes export scripts that write both the R-backed data and the raw workbook reference into JSON contracts under `migration-data/`. That should be treated as the handoff layer between the existing R system and the new application.
