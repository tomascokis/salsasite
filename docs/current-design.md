# Current Project Design

This document describes what the project currently does and how its main pieces fit together. It is a current-state design reference, not a replacement for the area-specific product contracts in:

- [docs/shared-ui-audit.md](/Volumes/fastdata/server/salsasite-dev/docs/shared-ui-audit.md)
- [docs/move-pages.md](/Volumes/fastdata/server/salsasite-dev/docs/move-pages.md)
- [docs/move-editor.md](/Volumes/fastdata/server/salsasite-dev/docs/move-editor.md)
- [docs/video-library.md](/Volumes/fastdata/server/salsasite-dev/docs/video-library.md)
- [docs/media-library.md](/Volumes/fastdata/server/salsasite-dev/docs/media-library.md)
- [docs/dancers.md](/Volumes/fastdata/server/salsasite-dev/docs/dancers.md)
- [docs/metadata.md](/Volumes/fastdata/server/salsasite-dev/docs/metadata.md)
- [docs/unraid-deployment.md](/Volumes/fastdata/server/salsasite-dev/docs/unraid-deployment.md)

## Purpose

The project is a salsa partnerwork encyclopedia and practice tracker. It catalogs moves, organizes them into the dense overview layout used by the original static site, shows move detail pages with relationships and videos, tracks learning progress snapshots, and provides tools for managing modern source videos, rendered move clips, dancers, topics, families, and move edits.

The repository now contains the SvelteKit application plus preserved legacy data/reference artifacts. The legacy R/Quarto implementation files have been retired; R and Quarto are not part of the active build, export, or runtime path.

Media catalog reads are split from media catalog repair. Ordinary media read models load the SQLite media catalog without mutating it; legacy move-video bootstrap, missing generated-variant pruning, generated cleanup repair, orphan draft relinking, point-in-time JSON catalog export, and read-only catalog diagnostics are explicit operator actions exposed from `/settings/media`. Production catalog mutations go through the serialized media catalog repository, and public catalog reads wait for queued writes before returning. This keeps live page/API reads predictable while preserving the current one-Docker, live-edit architecture.

## High-Level Architecture

```text
data/legacy reference inputs + data/live/bootstrap seeds + live media files
        |
        | first-run imports and live SQLite state
        v
data/live/app-state.sqlite
        |
        | SvelteKit server reads SQLite and configured media roots
        v
SvelteKit routes, APIs, media streaming, clip rendering
        |
        v
Browser UI
```

Important directories:

- `data/live/`: live SQLite state, bootstrap seeds, media roots, media trash, and media catalog exports.
- `data/legacy/`: preserved non-runtime RDS/XLSX reference inputs.
- `frontend/`: SvelteKit application.
- `docker/`: Unraid-oriented container setup.
- `docs/`: design notes, contracts, and deployment/workflow documentation.

## Retired Legacy Data Artifacts

The historical R/Quarto source files have been removed from the active repository. The data they produced or informed is preserved as data/reference material:

- `data/legacy/rds/dt_pw.RDS`, `dt_pw_lay.RDS`, and `progress.RDS`: preserved RDS source artifacts.
- `data/legacy/reference/data_reference.xlsx`: workbook-derived move reference data used by the app-native export helper.
- `data/live/bootstrap/**/*.json`: checked-in first-run SQLite seed contracts.

R/Quarto is no longer expected to regenerate these artifacts. The checked-in `_site_reference/` static output has also been retired from the repository. Future data refresh work should use app-native import/export tooling or a newly documented migration utility.

## Data Bootstrap Layer

`data/live/bootstrap/` contains checked-in bootstrap contracts imported into SQLite when the relevant tables are empty. These files are seed artifacts, not live stores and not an active R/Quarto export pipeline.

Current bootstrap files include:

- `bootstrap/catalog/manifest.json`: generation metadata, source references, route map, and counts.
- `bootstrap/catalog/moves.json`: normalized move records.
- `bootstrap/catalog/layout.json`: overview/dashboard column layout.
- `bootstrap/catalog/progress.json`: progress snapshots with summary counts and per-move scores.
- `bootstrap/catalog/raw-moves.json`: workbook-derived move reference data.
- `bootstrap/app-state/video-library.json`: media catalog bootstrap seed.
- `bootstrap/app-state/metadata.json`, `dancers.json`, and `move-edits.json`: editable app-state bootstrap seeds.
- `app-state.sqlite`: live SQLite state for migrated data-only editing workflows, media catalog tables, media jobs, and action history.

The current manifest reports 595 move rows, 12 layout columns, 7 progress snapshots, 540 trackable moves, and 59 moves with local video at export time.

## SvelteKit Runtime

The frontend app is in [frontend/](/Volumes/fastdata/server/salsasite-dev/frontend). It is a SvelteKit application using the Node adapter and Node 20 or later.

The app reads data from:

- `DATA_DIR`, defaulting to `../data/live`
- `MEDIA_ROOT`, defaulting to `../data/live/media/video-moves`
- `SOURCE_ROOT`, defaulting to `../data/live/media/video-sources`
- `POSTER_ROOT`, defaulting to `../data/live/media/video-posters`

[frontend/src/lib/server/data.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/data.ts) is the central loader for manifest, moves, layout, progress, search index, and raw move data. It reads the base catalog from SQLite, applies move edits from SQLite, and resolves video availability through the video library at request time.

[frontend/src/lib/server/paths.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/paths.ts) normalizes legacy `videomoves/` paths into the modern `video-moves/` namespace and protects media path resolution from escaping configured roots.

## Routes

The primary app shell is [frontend/src/routes/+layout.svelte](/Volumes/fastdata/server/salsasite-dev/frontend/src/routes/+layout.svelte). It provides navigation for:

- Overview
- Progress
- Media
- Dancers
- Create
- Settings

Implemented user-facing routes:

- `/`: dense multi-column move overview with search, variation/addition filters, summary counts, hover previews, and overflow handling.
  Overview overflow controls only appear for columns whose rows exceed the available screen height. When an overflowing column is expanded into an overflow column, the expanded state must show a single collapse control, not duplicate `See less` controls on both columns.
- `/moves/[slug]`: move detail page with metadata, inline detail editing, relationship diagram, videos, video variants, poster support, count overlays, and links back to source clip editors where available.
- `/moves/create`: move creation and editing workspace for drafts, published move edits, pinned moves, relationship editing, review flags, and draft publishing.
- `/progress`: historical progress snapshots rendered over the overview layout.
- `/progress/editor`: browser-side progress editor with keyboard controls and CSV import/export.
- `/media`: source-video media library with upload, filtering, pagination, publication-state indicators, posters, and source cards.
- `/media/edit/[id]`: source video metadata editor and move clip editor.
- `/dancers`: dancer list/profile area with editable dancer profiles and related dances/moves.
- `/topics/[slug]`: topic splash page that reuses the overview layout filtered to a topic.
- `/families/[slug]`: family splash page that reuses the overview layout filtered to a family.
- `/settings`: badge color settings stored in browser local storage.
- `/upload`: compatibility redirect area for older upload navigation.

Overview-style pages use the live published move dataset at runtime. Published moves and published move edits must appear on the home overview and overview-derived topic/family pages without requiring fresh bootstrap JSON. The saved overview layout defines topic/column placement, but move rows within a topic must be ordered from live catalog move metadata instead of stale saved layout row position.

## Move Model And Editing

Core move data is represented by `MoveRecord` in [frontend/src/lib/types.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/types.ts). A move includes identity, topic/family grouping, level/type, positions, tags, description, source/authorship, comments, review fields, ordering/layout fields, relationship IDs, validity, and video references.

The app treats SQLite catalog tables as the base catalog and layers local edits from SQLite state under `DATA_DIR/app-state.sqlite`. Existing JSON files under `data/live/bootstrap/` are used only to bootstrap empty SQLite tables.

[frontend/src/lib/server/move-editor.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/move-editor.ts) manages:

- Published move overrides.
- Created moves.
- Draft moves.
- Draft publishing.
- Relationship normalization so parent, child, and related links stay symmetric where appropriate.

The move detail page can edit the main details panel inline. The create/edit workspace supports drafts and published move edits, ID collision warnings, topic/family searchable picking, relationship diagram editing, pinned move context actions, and review flags.

## Relationship Diagrams

Move relationship diagrams are built from normalized move relationships. Parent, child, and related relationships are represented as graph edges and are rendered through shared relationship components.

The same relationship model is used in:

- Move detail pages for viewing connections.
- Move creation/editing for managing parent, child, and related move links.
- Topic/family filtered overviews where relationships still resolve against the full move catalog.

## Video And Media Library

The media system is split between [frontend/src/lib/server/media-catalog.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/media-catalog.ts), which owns SQLite catalog persistence, legacy `video-library.json` bootstrap, normalization, serialized writes, and snapshot export; catalog diagnostics, which owns read-only SQLite catalog health visibility; source, clip, render, bootstrap, repair, read-model, and job workflow services, which own catalog/media behavior; and [frontend/src/lib/server/video-library.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/video-library.ts), which is the stable public facade for route imports. Detailed media module boundary rules live in [docs/video-library.md](/Volumes/fastdata/server/salsasite-dev/docs/video-library.md). Together they manage:

- Source video assets under `video-sources/`.
- Move video assets under `video-moves/`.
- Legacy move video bootstrapping.
- Move-to-video links.
- Derived clips created from source videos.
- Clip rendering with `ffmpeg`.
- Low-resolution and padded low-resolution clip outputs.
- Clip publication to move pages.
- Runtime resolution of the videos shown on each move detail page.

The core product rule is that source videos do not appear directly on move pages just because they were uploaded. A source video must be sliced into a move clip, rendered, and published to moves before the move page receives the media link.

The source video editor supports metadata editing, clip range editing, action range editing, crop metadata, count markers, count overlay placement, timing presets, render status polling, and explicit publish-to-moves.

Posters are served from `POSTER_ROOT` and can be queued/generated for video assets.

## Progress Tracking

Progress data is loaded from SQLite after first-run import from `data/live/bootstrap/catalog/progress.json`.

The progress viewer builds snapshot views over the same layout as the overview page. Overview and progress move rows must show a small embossed selected-row animation after 0.1 seconds of mouseover, then show the shared move hover preview after 1 second of mouseover, and on keyboard focus, using the same compact video/name/ID preview treatment as relationship diagram move nodes. Shared move hover preview videos must fit within the preview frame without cropping, so portrait videos height-match the frame instead of being zoomed in. Each tracked move shows three colored status dimensions:

- Preparation
- Sequencing
- Success

The progress editor currently edits in the browser and supports:

- Keyboard navigation and scoring shortcuts.
- Per-dimension click cycling.
- CSV export.
- CSV import.
- Local change counting.

It is designed to become API-backed later; current edits are not persisted server-side unless exported/imported through CSV workflow.

## Metadata

[frontend/src/lib/server/metadata.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/metadata.ts) derives topics and families from moves and raw workbook references, then merges custom entries from SQLite state. Existing `data/live/bootstrap/app-state/metadata.json` content is imported when SQLite metadata tables are empty.

Metadata entries include names, slugs, descriptions, source type, timestamps, and move counts.

Topics and families have splash pages that reuse the overview renderer with a filter instead of becoming separate detail-page designs.

## Dancers

[frontend/src/lib/server/dancers.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/dancers.ts) derives dancer profiles from raw move references and video-library asset dancers, then merges custom dancer records from SQLite state. Existing `data/live/bootstrap/app-state/dancers.json` content is imported when SQLite dancer tables are empty.

Dancer profiles include:

- Full name
- Display name
- Instagram handle
- Role
- Level
- Region
- Related dances from source video assets
- Related moves from raw references and linked media

The `/dancers` page provides a searchable list, profile view, and editable profile form.

## APIs

The app exposes JSON endpoints for live workflows:

- `/api/search`: search built from the SQLite-backed move catalog.
- `/api/moves/[...id]`: save published move edits.
- `/api/moves/create`: save drafts, create drafts from names, and publish drafts.
- `/api/metadata`: save topic/family metadata.
- `/api/dancers`: save dancer profiles.
- `/api/history`: list recent recorded actions.
- `/api/history/[id]/undo`: undo supported data-only actions.
- `/api/media/library`: paginated media library data with filtering.
- `/api/upload/source`: upload source videos.
- `/api/upload/source/[id]`: update or delete source assets.
- `/api/upload/source/[id]/detect-fields`: infer source metadata fields.
- `/api/upload/clips`: save source-video clip definitions.
- `/api/upload/render`: queue clip renders and read render status.
- `/api/upload/publish`: publish rendered clips to moves.
- `/media/[...path]`: byte-range video serving from configured media/source roots.
- `/posters/[...path]`: poster serving.
- `/api/posters/[...path]`: poster status/generation support.

## UI Design

The UI favors dense, practical, dashboard-like screens over marketing-style pages. The overview and progress pages preserve the original multi-column browsing model. Shared reusable controls include:

- Searchable pickers.
- Editable list/grid components.
- Content/status badges.
- Context menus.
- Relationship diagram components.
- Overview splash renderer.
- Auto-resizing text areas.

Status and badge language is intended to stay shared across move pages, media browsing, and clip editing.

## Action History

[frontend/src/lib/server/app-state.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/app-state.ts) owns the SQLite database and bootstraps empty tables from `data/live/bootstrap/` seed files. [frontend/src/lib/server/history.ts](/Volumes/fastdata/server/salsasite-dev/frontend/src/lib/server/history.ts) lists and undoes supported actions.

The current undo-capable slice covers metadata entries, dancer profiles/deleted-profile markers, move drafts, published move edit overrides, and managed source-video deletes. Managed source-video delete undo depends on the media manager trash entries still existing under `DATA_DIR/media-trash/<job-id>/`. Source upload/update, clip save/key-video changes, clip render queueing, and clip publishing are audit-only action-history rows. Draft publishing actions are recorded but not undoable yet because they may involve media relinks. Poster generation, source hash backfill, generated cleanup, media job retry, and managed media renames are observable through media-manager jobs where applicable, but they are not user-history undo actions in this slice.

## Deployment

The intended deployment is a single Node container on Unraid. The preferred development/deployment shape is a live-mounted container where the repo is mounted into `/server/live` and the container supplies Node dependencies plus `ffmpeg`.

The current compose file maps:

- Host repo root to `/server/live`
- Container port `5173` to host port `18096`
- `DATA_DIR=/server/live/data/live`
- `MEDIA_ROOT=/server/live/data/live/media/video-moves`
- `SOURCE_ROOT=/server/live/data/live/media/video-sources`
- `POSTER_ROOT=/server/live/data/live/media/video-posters`

For browser verification from this checkout, use `http://192.168.0.127:18096`.

## Current Limitations And Boundaries

- The legacy R/Quarto implementation and checked-in static reference output have been retired. Preserved RDS/XLSX artifacts remain for audit and migration context, but they are not an active build/export path.
- The media catalog is SQLite-backed. JSON media catalog files are bootstrap/export artifacts, not the live source of truth after first SQLite bootstrap.
- Progress editor changes are currently browser-local unless imported/exported through CSV.
- Authentication, authorization, admin pages, and access tracking are planned but not implemented in the inspected app.
- Media render jobs are in memory. If the container restarts during rendering, the saved clip definition remains but the active render job is lost.
- The repository has existing uncommitted changes; this document does not attempt to reconcile or validate all pending worktree changes.
