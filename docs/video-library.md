# Video Library And Clip Editor

This document describes how the app stores videos, links videos to moves, uploads source recordings, renders clips, and generates posters.

Shared media status and badge behavior must follow `docs/shared-ui-audit.md`.

## Mental Model

The app has two video concepts:

- `source` videos are full dance or source recordings managed through `/media`.
- `move` videos are playable clips shown on move detail pages.

Source videos are not shown directly on move pages. A source video must be sliced and rendered into one or more move clips before it appears on a move detail page.

The source of truth for video relationships is the media catalog tables in the app SQLite database:

```text
DATA_DIR/app-state.sqlite
```

`DATA_DIR/video-library.json` is a one-time bootstrap seed and backup/export artifact. After the first successful SQLite bootstrap, it is not a live-edited catalog and changes to that JSON file are not picked up by ordinary media reads.

The exported move data still defines the encyclopedia. The video catalog only defines media assets, move-to-video links, and clip definitions.

Media filesystem actions must be routed through the server-side media manager rather than being open-coded in routes or page loaders. The media manager records durable jobs and file-action rows in `DATA_DIR/app-state.sqlite` alongside the SQLite-backed media catalog.

Managed source deletion must move source videos, rendered outputs, and matching poster sidecars into `DATA_DIR/media-trash/<job-id>/` instead of permanently unlinking them. The source-delete action must be recorded in action history with enough catalog state and media job state to undo the delete while those trash entries remain available.

Catalog-owned generated media cleanup and managed video renames must also run through media-manager jobs. This includes stale generated legacy assets, removed source-clip outputs, obsolete render outputs, and display-ID-driven generated file renames. Temporary duplicate-upload files may be permanently removed, but the temp delete must still be recorded as a media-manager file action.

The media manager page at `/settings/media` must expose recent jobs with their file-action rows for operational review. File-action details shown through the API or UI must use managed relative paths and sanitized metadata; absolute filesystem paths such as original and destination absolute paths must not be exposed in browser responses.

Media catalog repair is an explicit operator workflow on `/settings/media`. Repair scans must be dry-run only and must not mutate the SQLite catalog, move files, create media jobs, or record history. Applied repair runs may prune missing generated variant paths, add legacy move assets/links, clean stale generated assets, and relink orphaned generated draft IDs; applied runs must record media-manager file actions where files move or rename and must record a non-undoable `media.catalog.repair` history row.

Media catalog JSON export is an explicit operator workflow on `/settings/media`. Export writes a point-in-time snapshot under `DATA_DIR/media-catalog-exports/` for backup, debugging, or comparison. Exported JSON is not watched, not re-imported automatically, not a second source of truth, and not an undo workflow.

Media catalog diagnostics are read-only operator visibility on `/settings/media`. Diagnostics may report SQLite bootstrap state, catalog counts, latest export, missing files, missing references, invalid managed paths, and duplicate normalized asset paths. Diagnostics must not repair catalog rows, write exports, create media jobs, record action history, move files, or expose absolute filesystem paths in browser/API responses.

Production catalog mutations must go through `mutateMediaCatalog()` so SQLite catalog writes are serialized. Public `readMediaCatalog()` calls wait for any queued catalog write before returning a snapshot. Direct `writeMediaCatalog()` usage is reserved for the repository implementation and focused repository tests.

## Runtime Setup

In the live Unraid setup the repo is mounted once at `/server/live`. All video paths should point inside that mount.

| Purpose | Env var | Container path | Host path |
| --- | --- | --- | --- |
| App-managed SQLite/JSON state | `DATA_DIR` | `/server/live/migration-data` | `/mnt/user/fastdata/server/salsasite-dev/migration-data` |
| Playable move clips | `MEDIA_ROOT` | `/server/live/video-moves` | `/mnt/user/fastdata/server/salsasite-dev/video-moves` |
| Uploaded source videos | `SOURCE_ROOT` | `/server/live/video-sources` | `/mnt/user/fastdata/server/salsasite-dev/video-sources` |
| Poster images | `POSTER_ROOT` | `/server/live/video-posters` | `/mnt/user/fastdata/server/salsasite-dev/video-posters` |

The live container command should include:

```sh
-e DATA_DIR=/server/live/migration-data \
-e MEDIA_ROOT=/server/live/video-moves \
-e SOURCE_ROOT=/server/live/video-sources \
-e POSTER_ROOT=/server/live/video-posters \
-v /mnt/user/fastdata/server/salsasite-dev:/server/live
```

The dev image includes `ffmpeg`. Rendering clips and generating posters should normally happen inside the container.

## Catalog Model

The SQLite media catalog represents the same three logical collections that the legacy `video-library.json` seed contains.

| Array | Purpose |
| --- | --- |
| `videoAssets` | One row per real video file. |
| `moveVideoLinks` | Ordered links from move IDs to playable move assets. |
| `derivedClips` | Saved clip definitions created from source videos. |

### `videoAssets`

Each asset represents a real file under `video-moves/` or `video-sources/`.

Important fields:

- `kind`: `move` or `source`.
- `filePath`: managed path such as `video-moves/ABC00001 example.mp4`.
- `displayName`: human-readable label.
- `originalFilename`: uploaded or discovered filename.
- `dancers`: free-text list.
- `timing`: required enum, `on1`, `on2`, or `other`.
- `contentType`: required enum, `music`, `counts`, or `other`.
- `environment`: required enum, `social` or `class`.
- `recordDate`: optional `YYYY-MM-DD` date for when the source was actually recorded.
- `classWorkshop`: legacy optional source-context text retained in stored data, but not shown or edited in the media editor UI.
- `tags`: optional miscellaneous source tags such as `low quality`.
- `notes`: optional text.
- `contentHash`: optional source byte fingerprint stored as `sha256:<hex>`.
- `contentHashAlgorithm`: currently `sha256` when a byte fingerprint is known.
- `contentSizeBytes`: source file size captured with the byte fingerprint.
- `hashStatus`: `pending`, `ready`, or `failed`.

Existing legacy files get metadata inferred from their filenames where possible.

Source assets keep their normal catalog `id` as the app identity. `contentHash` is an immutable byte fingerprint used for duplicate detection and recovery, not the primary key. Two files with the same SHA-256 hash and byte size are treated as the same exact source material. Remuxed or transcoded copies may have different byte hashes and require a later video fingerprint if softer duplicate matching is needed.

### Media Editor Metadata Controls

- The Source metadata label in the media editor should use accent-colored text instead of muted grey.
- Timing, Type, and Environment segmented buttons should render the configured badge color when selected and a 50% mix with white when not selected.
- The media editor should not show a class/workshop field.
- The Dancers control in the media editor should use the shared searchable picker pattern against known dancers, and selected dancers should appear as a badge list rather than a comma-separated text field.

### `moveVideoLinks`

Move pages read these links to decide which videos to show. This is what enables one video asset to be used by many moves.

Only links to `move` assets are shown on move pages. Links to `source` assets are ignored intentionally.

### `derivedClips`

Each derived clip stores both the rendered clip range and the actual move range:

- `startMs` and `endMs` define the full rendered clip, including viewer context.
- `actionStartMs` and `actionEndMs` define where the move itself happens inside that clip.
- `sourceAssetId` points to the uploaded source video.
- `outputAssetId` points to the rendered move video after render succeeds.
- `actionOutputFilePath` points to the high-quality unpadded/action output after render succeeds.
- `status` is `pending`, `rendering`, `ready`, or `failed`.

## Legacy Move Video Import

Existing files under `video-moves/` continue to work after an explicit media catalog repair.

When an operator scans or runs catalog repair, the server scans `MEDIA_ROOT` for:

```text
.mp4 .m4v .mov
```

If a filename starts with a known move ID, the repair can create or reuse a `move` asset and create a move link.

Example:

```text
video-moves/BK020201 Block unwrap via hip [on2, music].mp4
```

This links the file to move `BK020201`.

The repair process is additive for legacy move videos. It does not delete catalog rows for missing legacy files.

## Media Workflow

The Media page is:

```text
/media
```

`/upload` redirects to `/media` for compatibility with older links.

Clicking a source card opens:

```text
/media/edit/[source-asset-id]
```

### Media Gallery Layout Contract

- Source cards and the add-source tile must use the same stable card track size.
- Media gallery source cards should use a compact card track, about three quarters of the previous large-card width.
- Compact source cards should prioritize the poster and title; secondary metadata may be hidden to keep the card footprint small.
- Resizing the window should add or remove source-card columns without making existing cards stretch into oversized tracks.
- Source posters must keep a fixed `16 / 9` frame and fit the poster image inside that frame.

The intended workflow is:

1. Upload a source video.
2. Enter required metadata: timing, type, and environment.
3. Select the source video in the library.
4. Add one or more move clips in the editor.
5. Save clip definitions.
6. Render selected clips.
7. Open the relevant move page and confirm the rendered clip appears as a tabbed video.

Uploading stores the original file in `video-sources/` and creates a `source` asset. It does not automatically link that full source video to move pages.

## Publish-To-Moves Contract

- Source media upload and metadata editing still do not link the full source video directly to move pages.
- A source video must not appear directly on a move page just because it was uploaded.
- New move clips start unpublished while they are only saved definitions.
- Rendered move clips publish to their move pages automatically as part of the normal save/render flow.
- Ready clips that are saved without needing a new render publish automatically during that save.
- Move pages receive media clip links only after a clip has rendered successfully.
- Modern move metadata inherited from parent media should update on move pages automatically when a ready clip is saved or rendered.
- Derived clips may persist a key-video flag that promotes them into the move page's limited main tab set when a move has many videos.
- New derived clips default to key videos for a move until that move has four clips; later clips default to non-key unless promoted manually.
- Source-video clip editor key-video star controls must live on the move rows as compact icon-sized buttons, including the active edited saved row, and must not appear as a separate top-level timeline toolbar button.
- Low-res and padded files generated for a derived clip are playback variants of that clip; they must not be bootstrapped as separate standalone move-page videos.
- Re-rendering or changing a derived clip must replace the previously published/generated clip assets and links instead of accumulating duplicate move-page videos.
- Generated clip cleanup must prove app ownership from the generated filename shape and current source/clip metadata where available; token-only filename matching is not sufficient for deleting media.
- Move pages must default to the full-quality rendered clip; low-res variants are optional alternates only, and the full rendered clip must preserve source quality as much as practical.
- Move-page tab metadata for derived clips must come from the source media metadata, not fallback `Other / Other / Class` bootstrap defaults.
- Legacy direct video links may be replaced only where an explicit migration workflow allows it.

## Clip Naming Contract

- Clip names should be generated automatically unless the user manually names the clip.
- A generated clip name should respond to source metadata changes.
- Once a clip has been manually named, automatic naming must stop overwriting that name.

## Clip Editor

The editor has one timeline and two ranges.

| Range | Meaning |
| --- | --- |
| Move range | The exact part where the move happens. |
| Clip range | The wider rendered clip, including context before and after the move. |

New clips start with context around the move. The outer clip markers follow the move markers until the user manually edits that side. Saved clips reopened for editing must preserve their current padding distances and keep the same follow behavior until the user manually edits a clip edge.

Current constraints:

- The left clip marker must remain before the move start.
- The right clip marker must remain after the move end.
- Each side keeps about `0.5s` separation where the video boundaries allow it.
- If the move starts near `0.00s`, the left clip marker clamps to `0.00s`.
- If the move ends near the video end, the right clip marker clamps to the video duration.
- Draft move editor rows must keep the Start time, Moves picker content, and Edit control in separate aligned columns; saved clip rows and selected move chips must not inherit generic button chrome that creates nested boxes or visual overlap.
- Draft and saved move editor rows must keep their columns aligned at desktop and tablet widths; if the viewport cannot fit the full row, the row area should scroll horizontally or use a deliberate narrow layout rather than allowing header labels and row controls to auto-place unpredictably.
- In active edit mode, the video/timeline column must either reserve enough width for the fixed editor row grid or stack separately from the metadata panel; timeline and row controls must not render underneath the metadata panel.
- In edit mode, the move clip row list must be a fixed five-row scroll window by default so the source video and timeline stay aligned with a stable editor height even when there are more clips.
- In edit mode, the move clip row list should show only the currently clicked move plus the three other move rows closest to the video playhead, rather than every saved move row in the source video.
- In edit mode, green saved clip bars in the lower timeline lane must continue to show all eligible saved clips, not only the filtered row-window clips.
- Clicking a timeline clip whose row is outside the current edit-mode row window must auto-scroll that internal row window to the selected row without scrolling the whole page.
- Timeline move ranges that are currently open for editing must remain orange, while move ranges that are not currently open for editing must appear green.
- The saved clip range containing the current playback position must be shown in a lighter green than the other saved clip ranges.
- Timeline ranges must not show small circular changed or unsaved badges; unsaved state belongs in the Save action and status text instead.
- Timeline playhead handles must render as complete circles without clipping in both compact playback mode and expanded draft-editing mode.
- Timeline playhead handles must exactly fill the vertical gap between the playback track and lower clip lane, using the measured lane geometry.
- Saved and secondary clip ranges must render on a lower lane below the playhead handle so they do not overlap the playback track marker, including in expanded edit mode.
- Timeline and edit-row animations must be smooth, restrained, and visibly paced rather than snappy; direct marker/playhead dragging must remain immediate without trailing transitions, and reduced-motion preferences must disable nonessential motion.
- Playback-nearest edit-row updates must settle briefly before changing the visible row window, so ordinary playback does not constantly reshuffle rows; when the settled window changes, rows should slide smoothly rather than jump.
- Clicking `Edit` on a different saved or draft row must keep the underlying row identity stable so the row window slides/reflows smoothly instead of tearing down and rebuilding rows.
- When a saved green move range is opened for editing, it should lift from the lower lane into the active timeline lane while turning orange; the clip padding range and trim/move markers must fade in only after that lift completes.
- When dragging a move start or move end marker, crossing another move boundary or landing within a small screen-precision tolerance of it must snap to that boundary once for that drag. After that first snap, moving away during the same drag must not keep snapping.
- One or more unsaved draft move rows must not be lost by clicking away, switching clips or sources, exiting the draft editor, or browser/page navigation without an explicit discard confirmation.
- Background render completion must update render status in place without refreshing or replacing editor state, exiting draft-editing mode, or discarding one or more active draft move rows.

Playback behavior:

- Spacebar toggles play/pause unless focus is inside an input, textarea, select, button, or editable element.
- The playhead can be dragged even before a clip is being edited.
- When a clip enters draft editing, loop playback and the loop's `With padding` mode must both start enabled by default.
- In draft editing, the plain loop mode must cover only the move range, while `With padding` must extend that loop to the clip's head and tail padding.
- In playback mode, when a source video has saved move clips, a compact centered boxed move-context strip must appear above the timeline without visible labels: the center current-move box is always visible and shows `—` when the playhead is not within a move range, while previous/next boxes appear only when that move ended or starts within 2.5 seconds of the playhead.
- The playback move-context strip must reserve stable left, center, and right slots at every viewport width so context boxes never stack vertically and the current-move box remains centered even when only previous or next is visible.
- Previous and next move boxes in the playback move-context strip must be visibly shorter than the center current-move box.
- The center current-move box must replace move names without rendering the outgoing and incoming names at the same time, so transitions never change the strip height.
- The source-video playback controls must stay compact but visually polished, with clear icon affordances and a refined volume treatment.
- The source-video playback controls and move-context boxes must stay vertically compact so they do not consume unnecessary timeline height.
- The editor attempts playback from `0.00s` when a source video loads, using the shared site mute preference. On a fresh site load the player should default to unmuted, while browser autoplay policy can still reject autoplay so manual play remains available.
- The source-video mute button must directly toggle muted/unmuted audio and persist that choice through the shared site mute preference.
- Timeline positions fall back to saved clip times while video metadata is still loading.

Default draft timing:

- New draft move ranges must default to about `2.5s`, half the previous default length.
- New draft clips must default to head padding equal to the move length.
- New draft clips must default to tail padding equal to half the move length.

## Rendering

Saved clip definitions do not create move videos until they are rendered.

Rendering is handled by the app server with `ffmpeg` through durable media-manager `clip.render` jobs. The queue still runs inside the single live Docker container, but job state is stored in SQLite so interrupted render jobs can be retried after restart.

The render command is effectively:

```text
ffmpeg -ss <start> -t <duration> -i <source> -c:v libx264 -preset <profile> -crf <quality> -c:a aac -movflags +faststart <output>
```

High-quality outputs use the full-quality render profile. Low-resolution preview outputs may use a lower-quality profile and scale down for preview use only.

High-quality output files are written to:

```text
video-moves/<moveId> <source display name> <clip-id-prefix>.mp4
video-moves/<moveId> <source display name> <clip-id-prefix> action.mp4
```

The first file is the padded/full-context clip from `startMs` to `endMs`. The `action` file is the high-quality unpadded move-action clip from `actionStartMs` to `actionEndMs`, falling back to `startMs` and `endMs` when action timing is absent.

Low-resolution `low` and `padded low` files may also be generated for previews or fallbacks, but move pages must use high-quality clips for normal playback and expose both high-quality padded and high-quality action versions.

When render succeeds:

- The clip status becomes `ready`.
- A `move` video asset is created or updated.
- The rendered clip is automatically published to the target move.
- A `moveVideoLinks` row links the output asset to the target move.
- A high-quality action-range output is saved alongside the padded output.
- Smaller-resolution preview outputs may be generated for later use.
- Poster generation is queued.

When render fails:

- The clip status becomes `failed`.
- The failure message is stored on the clip.
- The clip can be rendered again after fixing the cause.

If the container restarts while a job is rendering, the media manager recovers the interrupted job to `queued` so it can be retried. The saved clip definition remains in the SQLite media catalog.

## Move Page Resolution

Move detail pages receive a resolved `videos` view model assembled at runtime.

The resolver combines:

- legacy move videos discovered under `video-moves/`
- catalog move assets linked through `moveVideoLinks`
- rendered derived clips that have `status: "ready"`

For derived clips, the move page shows inherited provenance from the source asset:

- dancers
- timing
- content type
- environment
- source label
- notes

Derived clip definitions may also store expression descriptors:

- an optional extra label for this clip's expression of the move
- optional start and end position IDs from the strict position option set
- an optional timing group ID for rows intentionally bound to the same clip/action timing

The extra label may be shown in clip display contexts such as the media editor row and move-page video tab, but it must not change the canonical move name.

The current tabbed player UI uses this resolved view model. It does not read source videos directly.

## Posters

Posters are stored under `POSTER_ROOT` using the same managed path as the video, with an image extension.

Example:

```text
video-moves/BK020201 example.mp4
video-posters/video-moves/BK020201 example.jpg
```

For source videos:

```text
video-sources/full dance.mp4
video-posters/video-sources/full dance.jpg
```

The app checks these extensions:

```text
.jpg .jpeg .webp .png .avif
```

Poster generation is queued automatically for uploaded sources and rendered clips when `ffmpeg` is available. Poster requests must create or reuse durable `poster.generate` media-manager jobs before running `ffmpeg`.

Bulk-generate move posters on Unraid:

```sh
docker exec salsasite-dev bash /server/live/scripts/generate_video_posters.sh \
  /server/live/video-moves \
  /server/live/video-posters/video-moves \
  1.0
```

Bulk-generate source posters on Unraid:

```sh
docker exec salsasite-dev bash /server/live/scripts/generate_video_posters.sh \
  /server/live/video-sources \
  /server/live/video-posters/video-sources \
  1.0
```

## API Summary

| Route | Method | Purpose |
| --- | --- | --- |
| `/api/upload/library` | `GET` | Return legacy upload library data. |
| `/api/media/library?limit=50&cursor=...` | `GET` | Return paginated Media page source cards, grouped by upload month, with tag/class suggestions. |
| `/api/media/jobs?limit=100` | `GET` | Return recent media-manager jobs with sanitized file-action details. |
| `/api/media/jobs` | `POST` | Queue media-manager maintenance actions such as source hash backfill. |
| `/api/media/jobs/[id]/retry` | `POST` | Retry supported failed media jobs. |
| `/api/upload/source` | `POST` | Upload a source video and create a source asset. |
| `/api/upload/source/[id]` | `PUT` | Update source metadata. |
| `/api/upload/source/[id]` | `DELETE` | Move source, derived clips, rendered outputs, links, and posters through managed delete/trash. |
| `/api/upload/clips` | `POST` | Save clip definitions for a source asset. |
| `/api/upload/render` | `POST` | Queue render jobs. |
| `/api/upload/render?ids=...` | `GET` | Poll render status. |
| `/media/[...path]` | `GET` | Serve move/source videos with byte-range support. |
| `/posters/[...path]` | `GET` | Serve poster images. |

## Deletion Semantics

Deleting a source video removes catalog references for:

- derived clip definitions from that source
- move links for those rendered move clips
- catalog asset rows for the source and rendered outputs

Deleting a source video moves these files into media-manager trash instead of permanently unlinking them:

- the source video file
- source poster files
- rendered move clips made from that source
- posters for those rendered move clips
- generated low-resolution preview variants for the affected derived clips

Source-delete undo restores the catalog rows and moves the trashed files back to their original managed locations when the trash files are still available under `DATA_DIR/media-trash/<job-id>/`. If required trash files are missing, undo must fail without restoring the catalog rows.

Deleting a source does not remove unrelated legacy move videos.

Removing clips, re-rendering clips, legacy generated cleanup, and move display-ID sync may move or rename generated files and posters. Those file operations are durable media-manager actions, but they are not user-facing history undo actions in this slice.

The `/settings/media` file-action drilldown is for visibility and debugging. It does not make generated cleanup user-history undoable.

## Mutation Audit Classification

Media/catalog mutations must be classified before implementation:

| Workflow | History classification | File-operation classification |
| --- | --- | --- |
| Metadata, dancer, move draft, and move edit saves | Undoable action history | Not applicable |
| Managed source-video delete | Undoable action history while media trash exists | Media-manager trash-backed file actions |
| New source upload | Audit-only action history | `source.hash` media job records the source write |
| Source metadata update | Audit-only action history | Not applicable |
| Source clip definition save and key-video toggle | Audit-only action history | Removed generated clips are media-manager cleanup actions |
| Clip render queue and clip publish | Audit-only action history | Render outputs and posters are media-manager jobs/file actions |
| Applied media catalog repair | Audit-only action history | Generated cleanup and renames are media-manager jobs/file actions |
| Duplicate-upload temp cleanup, poster generation, source hash backfill, media job retry, generated cleanup, obsolete-render cleanup, and display-ID renames | No action-history row | Media-manager operational jobs/file actions |

Audit-only media history rows are listed in `/settings/history` for review but are intentionally non-undoable. Generated cleanup and managed renames remain observable through `/settings/media` instead of becoming user-history undo workflows.

## What Requires A Container Rebuild

With the live-mounted setup, these do not require rebuilding the Docker image:

- Svelte frontend source edits
- server route edits
- upload page/editor edits
- media catalog table changes through the running app
- adding or deleting videos
- adding or deleting posters

These do require rebuilding or restarting depending on the change:

- changes to `Dockerfile.dev`
- installing new OS packages
- changing container env vars
- changing port mappings
- changing the mounted host path

## Troubleshooting

`ffmpeg is required on PATH`

Run render/poster commands inside the live Docker container unless `ffmpeg` is installed on the Unraid host.

Move page does not show a rendered clip

Confirm the clip status is `ready`, the output file exists under `video-moves/`, the clip move ID is valid, and `moveVideoLinks` links the move ID to the output asset.

Legacy video file does not link to a move

Confirm the filename starts with a valid move ID and has one of the scanned extensions: `.mp4`, `.m4v`, or `.mov`.

Uploaded source appears on Media but not on a move page

This is expected. Source videos must be sliced and rendered before they appear on move pages.

Posters do not appear

Confirm the matching poster exists under `video-posters/`, `ffmpeg` is available in the container, and `docker logs salsasite-dev` has no poster generation errors.

Timeline markers stack at the left or will not move

Confirm the browser loaded the latest frontend bundle, then hard refresh. Also confirm `/media/...` video requests return byte ranges, because browser seeking depends on range support.

Docker cannot see files

Confirm the single live mount exists:

```text
/mnt/user/fastdata/server/salsasite-dev:/server/live
```

Confirm env vars point inside `/server/live`:

```text
DATA_DIR=/server/live/migration-data
MEDIA_ROOT=/server/live/video-moves
SOURCE_ROOT=/server/live/video-sources
POSTER_ROOT=/server/live/video-posters
```

## Important Files

| File | Purpose |
| --- | --- |
| `frontend/src/lib/server/media-catalog.ts` | SQLite media catalog persistence, legacy `video-library.json` bootstrap, normalization, and serialized writes. |
| `frontend/src/lib/server/media-source-service.ts` | Source upload, metadata update, delete/restore, duplicate cleanup, and source hash workflows. |
| `frontend/src/lib/server/media-clip-service.ts` | Clip save, key-video update, publication, display-ID rename, and clip catalog mutation workflows. |
| `frontend/src/lib/server/media-render-service.ts` | Clip render queueing, ffmpeg orchestration, render file actions, obsolete render cleanup, and render status mutation. |
| `frontend/src/lib/server/media-bootstrap-service.ts` | No-write catalog reads plus compatibility repair loading for legacy callers. |
| `frontend/src/lib/server/media-repair-service.ts` | Explicit scan/apply catalog repair for legacy move-video bootstrap, generated cleanup repair, missing variant pruning, and orphan draft relinking. |
| `frontend/src/lib/server/media-read-models.ts` | Side-effect-free move video, upload, media library, render status, and summary view models. |
| `frontend/src/lib/server/media-job-service.ts` | Media-manager job listing and supported job retry glue. |
| `frontend/src/lib/server/video-library.ts` | Stable compatibility facade that re-exports media server APIs used by routes. |
| `frontend/src/lib/server/posters.ts` | Poster lookup and async poster generation. |
| `frontend/src/lib/server/paths.ts` | Runtime path resolution and managed path safety. |
| `frontend/src/routes/media/+page.svelte` | Media gallery with source cards and upload tile. |
| `frontend/src/routes/media/+page.server.ts` | Media page data load. |
| `frontend/src/routes/media/edit/[id]/+page.svelte` | Source metadata and move-clip editor for one uploaded source. |
| `frontend/src/routes/upload/+page.server.ts` | Compatibility redirect to `/media`. |
| `frontend/src/routes/api/upload/*` | Media management API endpoints. |
| `frontend/src/routes/media/[...path]/+server.ts` | Byte-range video serving. |
| `frontend/src/routes/posters/[...path]/+server.ts` | Poster serving. |
| `scripts/generate_video_posters.sh` | Bulk poster generation helper. |

## Media Module Boundaries

Media routes should import server media APIs through `frontend/src/lib/server/video-library.ts`. That file is a compatibility facade and should remain re-export-only.

Media domain modules must not import the `video-library.ts` facade. They should import the specific peer service or lower-level module they need. `media-catalog.ts` owns SQLite catalog persistence and the one-time legacy JSON bootstrap; source, clip, render, bootstrap, repair, read-model, and job services own their named workflow areas; `media-manager.ts` owns durable media jobs and file-action rows.

Ordinary page/API read models must use the no-write media catalog path. Legacy bootstrap and repair routines that can mutate the SQLite media catalog must be invoked explicitly through the media repair service or the compatibility bootstrap repair path, not hidden inside read-model assembly. The compatibility `getVideoLibrary(moves)` export preserves the existing repair-capable behavior for callers that intentionally need it, while `readVideoLibrary()` is the side-effect-free catalog read.

Workflow modules should use `mutateMediaCatalog()` for catalog changes and should not import `writeMediaCatalog()` directly. If a workflow needs file operations and catalog changes, it should validate the catalog state, execute managed file operations through `media-manager.ts`, and commit catalog changes through the serialized repository boundary with explicit failure behavior.

`media-workflow-helpers.ts` should stay limited to helpers shared by multiple media modules. Helpers used by only one workflow should live in that workflow module unless moving them would duplicate nontrivial logic or create a circular dependency.
