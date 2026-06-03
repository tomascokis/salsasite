# Plan 2.0: Unresolved Plan 1.0 Work

Plan 2.0 is the organized todo list for unresolved Plan 1.0 work after the current implementation review. It supersedes Plan 1.1 as the active follow-up plan.

This plan should finish the media-to-move workflow before starting migration matching, security, or admin activity tracking. Items that appear implemented in source but have not been browser-verified are called out as verification tasks rather than assumed-new feature work.

## Priority 1: Close The Media-To-Move Editing Loop

These items protect the core Plan 1.0 contract: media clips can be edited as drafts, rendered, published to moves, and revisited from move pages without ambiguity.

| ID | Todo | Source Plan IDs | Notes |
| --- | --- | --- | --- |
| P2-Media-1 | Make Go to Clip open the intended clip editing context | `MediaEdit-1`, `MoveView-7` | The move page links to `/media/edit/[source]?clip=[clip]&mode=clip`, and the editor selects the clip. Finish the behavior so it clearly enters the clip/move editing context rather than only selecting the row. |
| P2-Media-2 | Finish count placement controls in the media editor | `MediaEdit-15`, `MediaEdit-18`, `MediaEdit-19` | Expose visible controls to start count placement, place the current count, finish placement, and understand the active count. Enter-key placement exists in code, but the workflow needs discoverable UI. |
| P2-Media-3 | Add direct corner controls for count overlay placement | `MediaEdit-20` | Replace any dropdown-only placement flow with four explicit corner buttons: top-left, top-right, bottom-left, bottom-right. |
| P2-Media-4 | Expose count timing preset controls | `MediaEdit-21`, `MediaEdit-22`, `MediaEdit-23` | The preset data exists for On2, On2 all, On1, and On1 all. Add the editor UI and make preset changes rebuild the marker sequence intentionally. |
| P2-Media-5 | Finish clear-flag editing for count overlays | `MediaEdit-24` | Count markers can store a clear flag. Add a clear/hold editing control so overlays can disappear before the next count when needed. |
| P2-Media-6 | Verify count preview and runtime toggle end to end | `MediaEdit-25`, `MediaEdit-26` | Move pages have a runtime count toggle when markers exist, and the editor can preview active markers. Browser-verify save, render, move-page playback, and default-off behavior. |
| P2-Media-7 | Verify changed-unpublished detection for non-range edits | `MediaEdit-7` | Source now compares crop, count markers, count placement, and count preset. Confirm crop/count-only edits mark published clips as changed drafts and can be republished cleanly. |
| P2-Media-8 | Verify crop save and rendered output behavior | `MediaEdit-13`, `MediaEdit-14` | Server-side rendering now applies `cropRect`. Browser-verify crop drawing, saving, render output, and move-page playback variants. |

## Priority 2: Make Status And Metadata Contracts Obvious

These items reduce ambiguity between legacy move videos, modern media-derived clips, drafts, published clips, and inherited metadata.

| ID | Todo | Source Plan IDs | Notes |
| --- | --- | --- | --- |
| P2-Status-1 | Tighten shared status/badge usage across move and media surfaces | `Infra-2`, `Infra-3`, `MoveView-3`, `MediaView-4` | `ContentBadge` and `content-status.ts` exist, but pages still mix local badge classes and generic property badges. Normalize legacy, modern published, changed, never-published, processed, and unprocessed display. |
| P2-Status-2 | Clarify legacy-versus-modern metadata inheritance in the UI | `MoveView-4`, `MoveView-5` | Modern move metadata should read as linked to parent media and controlled by publish-to-moves. Legacy moves should read as holding their own stats. |
| P2-Status-3 | Audit move-page video variant labeling | `MoveView-6`, `MediaEdit-6` | Move pages expose padded, action, low-res action, and low-res padded variants. Confirm labels and defaults match the contract: full-quality padded by default, low-res as optional alternates only. |
| P2-Status-4 | Verify processed/unprocessed status display | `MediaEdit-9` | The shared status model has processed/unprocessed states. Confirm they are visible where clip processing state matters, not just represented in helper code. |

## Priority 3: Finish Shared UI Adoption

These items reduce duplicate UI behavior and keep Plan 1.0's shared-component direction intact.

| ID | Todo | Source Plan IDs | Notes |
| --- | --- | --- | --- |
| P2-UI-1 | Finish shared editable-list adoption | `Infra-5` | `EditableList.svelte` exists and is used in metadata/dancers, but some routes still hand-roll equivalent list/grid markup. Audit remaining surfaces and migrate only where it reduces duplication. |
| P2-UI-2 | Audit structured-field controls across all move edit surfaces | `MoveEdit-5` | Move creation and inline move editing use structured controls for positions, tags, and authorship. Check any remaining legacy edit route or secondary surface for freeform fields and either align or retire it. |
| P2-UI-3 | Keep context menu behavior shared | `Infra-6`, `MoveEdit-7`, `MoveEdit-8`, `MoveEdit-9`, `MoveEdit-10` | Context menus support right-click and long-press. Verify relationship actions still use the shared component everywhere rather than page-specific forks. |

## Priority 4: Migration Matching

Do not start this until Priority 1 and Priority 2 are closed. Migration relies on draft, render, publish, status, and replacement behavior being trustworthy.

| ID | Todo | Source Plan IDs | Notes |
| --- | --- | --- | --- |
| P2-Migration-1 | Select one original video and known legacy clips for a test set | `Migration-1` | Choose a small sample with known expected matches. |
| P2-Migration-2 | Build and time a legacy-clip matching prototype | `Migration-2`, `Migration-3`, `Migration-4` | Detect whether a trimmed legacy move clip appears inside a source video. Record accuracy and runtime before building UI. |
| P2-Migration-3 | Add Scan Legacy Clips workflow to media editing | `Migration-5`, `Migration-6` | Trigger scanning from the relevant media add/edit context after the prototype is reliable. |
| P2-Migration-4 | Create draft move tracks from matches | `Migration-7`, `Migration-8` | Matches should create draft modern move tracks and inherited draft metadata, not immediately replace visible move content. |
| P2-Migration-5 | Replace legacy direct links only after render and publish | `Migration-9` | Preserve the migration safety contract: no silent visible content replacement before a modern clip is rendered and published. |

## Priority 5: Security And Admin

Start this after the core content workflow and migration direction are stable, unless the site becomes publicly exposed earlier.

| ID | Todo | Source Plan IDs | Notes |
| --- | --- | --- | --- |
| P2-Security-1 | Add mandatory login and stay-logged-in behavior | `Security-1`, `Security-2` | Implement route-level protection rather than adding checks page by page. |
| P2-Security-2 | Track IP visits, login events, and content access | `Security-3`, `Security-4`, `Security-5` | Store timestamped activity records in the live persistence layer. |
| P2-Admin-1 | Add admin page shell and admin settings subpage | `Admin-1`, `Admin-2` | Keep existing settings pages distinct from true admin activity views. |
| P2-Admin-2 | Add admin activity summaries | `Admin-3`, `Admin-4`, `Admin-5` | Show timestamped IP activity, content access overview, and login activity in a scan-friendly format. |

## Completion Definition

Plan 2.0 is complete when:

- A move-page Go to Clip action lands in a clear clip editing state.
- Count placement can be fully configured, saved, previewed, rendered, and toggled at runtime.
- Crop and count-only edits reliably create changed-unpublished draft state and can be republished.
- Shared status badges make legacy, modern, draft, published, processed, and unprocessed states consistent across move and media pages.
- Modern metadata inheritance is understandable from the UI and stays controlled by publish-to-moves.
- Remaining shared UI adoption gaps are either closed or explicitly documented as intentional local exceptions.
- Migration matching has a validated prototype before any replacement workflow is added.
- Login/admin work is implemented only after core content workflows are stable or earlier if exposure risk requires it.
