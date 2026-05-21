# Salsa Site Implementation Plan

This document turns the feature notes into a sequenced implementation roadmap with stable action IDs. The intent is to keep design, data model changes, UI reuse, and publishing behavior aligned as the project moves from legacy move clips to modern media-linked move sections.

## Product Direction

The site should make the distinction between legacy and modern content clear without making the interface feel technical.

- Legacy moves come from the migration dataset and have direct clips attached.
- Modern moves are linked to clip sections from media.
- Modern moves inherit timing, type, environment, dancers, and other relevant metadata from their parent video/clip, but those changes only become visible on move pages when the user publishes them to moves.
- Draft media edits must be visible as drafts in the editor, but should not silently update published move pages.

The UI should be sleek, dense, and practical. Avoid placeholder forms. Reuse shared infrastructure for dropdown search, grid views, badges, context menus, filtered overview renders, editor panels, and status indicators.

## Visual Language

Use a consistent status model across moves and media.

- Timing, type, and environment should be badges, matching the media manager.
- Published content should look stable and neutral.
- Changed but unpublished clips should be marked as draft and use an orange border.
- Move clips that have never been published should use a yellow border.
- Legacy move clips should have a clearly distinguishable legacy marker where relevant.
- Relationship properties should be visible through the move diagram, not repeated as separate text fields.
- Review flags should be visible in the editor, with flagged content hidden by default.

## Shared Infrastructure

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| Infra-1 | Audit shared UI components | Identify existing dropdowns, grids, badges, diagrams, overview renders, timeline controls, and editor shell patterns. | None |
| Infra-2 | Define shared content status model | Cover legacy, modern draft, modern published, processed, unprocessed, changed unpublished, and never published states. | Infra-1 |
| Infra-3 | Add reusable badge component | Used for timing, type, environment, publication, processing, and legacy/modern markers. | Infra-1 |
| Infra-4 | Add reusable searchable picker | Used for topics, families, dancers, and regions. Prefer existing dropdown search code if available. | Infra-1 |
| Infra-5 | Add shared editable grid/list pattern | Support view-by-default with small edit affordances. | Infra-1 |
| Infra-6 | Add shared context menu interaction | Support right-click and long-press interactions for move relationship editing. | Infra-1 |
| Infra-7 | Add shared overview filter adapter | Allows the existing overview renderer to display filtered topic/family splash pages. | Infra-1 |

## Metadata Under Create

Add a metadata subpage under Create for managing topics and families.

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| Metadata-1 | Add metadata subpage under Create | Should sit naturally beside existing create tools. | Infra-5 |
| Metadata-2 | Add Topic list, create, and edit UI | Each topic has name and description. | Metadata-1, Infra-4 |
| Metadata-3 | Add Family list, create, and edit UI | Each family has name and description. | Metadata-1, Infra-4 |
| Metadata-4 | Add topic splash page | Reuse the overview render filtered to moves in the selected topic. | Metadata-2, Infra-7 |
| Metadata-5 | Add family splash page | Reuse the overview render filtered to moves in the selected family. | Metadata-3, Infra-7 |
| Metadata-6 | Add metadata persistence/API support | Only needed if topic/family CRUD is not already present. | Metadata-2, Metadata-3 |

## Move Viewing

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| MoveView-1 | Remove separate relationship property display | Users should read relationships from the diagram. | Infra-2 |
| MoveView-2 | Make relationship diagram the primary relationship view | Ensure parent, child, and related links are visually clear. | MoveView-1 |
| MoveView-3 | Show timing, type, and environment as badges | Match media manager styling. | Infra-3 |
| MoveView-4 | Model legacy-vs-modern metadata behavior | Legacy moves hold their own stats. Modern moves store their own stats but stay linked to video metadata. | Infra-2 |
| MoveView-5 | Keep modern move metadata updated from parent video | Updates should respect the publish-to-moves workflow. | MoveView-4, MediaEdit-5 |
| MoveView-6 | Add padded and unpadded low-res move video references | Server should hold smaller resolution variants with the move. | MediaEdit-6 |
| MoveView-7 | Add edit-mode Go to Clip button | Jumps to the media editor, opens edit moves, and selects the linked clip. | MediaEdit-1 |

## Move Editing

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| MoveEdit-1 | Add ID collision warning while typing | Should warn before save, not only fail at submit. | Infra-1 |
| MoveEdit-2 | Add searchable topic picker | Use shared dropdown search. | Infra-4, Metadata-2 |
| MoveEdit-3 | Add searchable family picker | Use shared dropdown search. | Infra-4, Metadata-3 |
| MoveEdit-4 | Replace freeform relation type fields with controls | Use segmented buttons for addition, variation, or neither where appropriate. | Infra-3 |
| MoveEdit-5 | Add relevant selection UI for other structured fields | Use selects, toggles, segmented controls, or buttons depending on field type. | Infra-3 |
| MoveEdit-6 | Add remove button beside pinned moves | Use a compact x/remove affordance. | Infra-5 |
| MoveEdit-7 | Add context menu to visible left-pane moves | Right-click and long-press should both work. | Infra-6 |
| MoveEdit-8 | Add context menu action: add as parent | Applies to the current move being edited. | MoveEdit-7 |
| MoveEdit-9 | Add context menu action: add as child | Applies to the current move being edited. | MoveEdit-7 |
| MoveEdit-10 | Add context menu action: add as related | Applies to the current move being edited. | MoveEdit-7 |
| MoveEdit-11 | Add flag-for-review support | Persist review flag and optional content. | Infra-2 |
| MoveEdit-12 | Show review section above pinned moves | Hidden by default, visible when expanded. | MoveEdit-11 |

## Dancers

Dancers should be viewable by default, with a small edit button to change parameters.

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| Dancer-1 | Add dancers page route and navigation | Should be a first-class content page. | Infra-5 |
| Dancer-2 | Add dancer data model | Full name, display name, and Instagram handle. | Dancer-1 |
| Dancer-3 | Add dancer role field | Lead or follow. | Dancer-2 |
| Dancer-4 | Add dancer level field | World class, pro, semi-pro, amateur, or unknown. | Dancer-2 |
| Dancer-5 | Add dancer region field | Region uses pre-existing options but allows typed custom values. | Infra-4, Dancer-2 |
| Dancer-6 | Build read-only dancer profile view | Edit affordance should be small and unobtrusive. | Dancer-2 |
| Dancer-7 | Add dancer-associated dances grid | Grid of dances associated with the dancer. | Dancer-6, Infra-5 |
| Dancer-8 | Add dancer-associated moves grid | Grid of moves associated with the dancer. | Dancer-6, Infra-5 |

## Media Viewing

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| MediaView-1 | Add published status filter | Include published, unpublished, and draft states. | Infra-2 |
| MediaView-2 | Add social/classes filter | Should support filtering media by type/use case. | Infra-4 |
| MediaView-3 | Add dancer filter | Uses shared dancer picker. | Dancer-2, Infra-4 |
| MediaView-4 | Align media filters with shared UI patterns | Filters should feel consistent with metadata and move editing. | MediaView-1, MediaView-2, MediaView-3 |

## Video And Clip Editing

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| MediaEdit-1 | Make media editor deep-linkable to a clip | Needed for Go to Clip from move pages. | Infra-1 |
| MediaEdit-2 | Auto-name clips unless manually named | Example: "Arti and Samara May class #3". | Dancer-2 |
| MediaEdit-3 | Update generated clip names when metadata changes | Should stop updating once a manual name is applied. | MediaEdit-2 |
| MediaEdit-4 | Make new clips unpublished by default | Publication should be explicit. | Infra-2 |
| MediaEdit-5 | Add Publish to Moves workflow | Metadata and links push down only when this button is pressed. | MoveView-4 |
| MediaEdit-6 | Generate low-res move videos | Generate padded and unpadded smaller-resolution versions. | MediaEdit-5 |
| MediaEdit-7 | Add draft state for changed published clips | Changed-but-unpublished clips get an orange border. | Infra-2, MediaEdit-5 |
| MediaEdit-8 | Mark never-published move clips | Never-published clips get a yellow border. | Infra-2, MediaEdit-5 |
| MediaEdit-9 | Add processed/unprocessed clip state | Clips start unprocessed and can later be marked processed. | Infra-2 |
| MediaEdit-10 | Add timeline zoom interaction | Scroll/pinch zoom in and out; starts zoomed out. | Infra-1 |
| MediaEdit-11 | Add clear zoom state indicator | It should always be visually clear when zoom is applied. | MediaEdit-10 |
| MediaEdit-12 | Add reset zoom button | Appears next to related timeline controls when zoom is active. | MediaEdit-10 |
| MediaEdit-13 | Add per-move crop tool | User can draw a rectangle for generated move videos. | MediaEdit-1 |
| MediaEdit-14 | Store crop metadata | Crop applies to generated videos for that move track. | MediaEdit-13 |
| MediaEdit-15 | Add count-placement mode | Accessed by a sensible button in the move clip editor. | MediaEdit-10 |
| MediaEdit-16 | Zoom timeline to current move in count mode | Include padding around the selected move track. | MediaEdit-15 |
| MediaEdit-17 | Add on2 default count sequence | Default sequence: 6, 7, 1, 2, 3, 5. | MediaEdit-15 |
| MediaEdit-18 | Add count placement workflow | User moves video player, presses Enter or Place, then advances to next count. | MediaEdit-15 |
| MediaEdit-19 | Add Finish button for count mode | Completes count placement. | MediaEdit-18 |
| MediaEdit-20 | Add count marker corner controls | Four buttons move the video overlay marker to each corner. | MediaEdit-15 |
| MediaEdit-21 | Add timing preset dropdown | Options include on2, on2 all counts, on1, and on1 all counts. | MediaEdit-15 |
| MediaEdit-22 | Add on2 all-count sequence | Adds 4 and 8 to the default on2 sequence. | MediaEdit-21 |
| MediaEdit-23 | Add on1 count sequences | Support 1, 2, 3, 5, 6, 7 and all-count equivalent. | MediaEdit-21 |
| MediaEdit-24 | Add clear flag for count overlays | Each count holds until the next count or a clear flag. | MediaEdit-15 |
| MediaEdit-25 | Preview count overlay in editor | Overlay is generated live from metadata. | MediaEdit-15 |
| MediaEdit-26 | Add runtime overlay toggle for videos | Overlay should never be enabled by default. | MediaEdit-25 |

## Migration

Legacy clips have already been precut from longer videos. The migration process should match original uploaded videos to legacy move videos that were made as trimmed versions.

### Phase 1: Test

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| Migration-1 | Select one test original video and known legacy clips | Use this to develop and validate the matching process. | None |
| Migration-2 | Build matching prototype | Detect whether a legacy trimmed clip appears inside the original video. | Migration-1 |
| Migration-3 | Track execution speed | Record runtime and scaling expectations. | Migration-2 |
| Migration-4 | Validate sample match accuracy | Confirm one sample video can be matched to moves. | Migration-2 |

### Phase 2: Implementation

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| Migration-5 | Add Scan Legacy Clips button to media page | Triggered when adding or editing a video. | Migration-4, MediaEdit-1 |
| Migration-6 | Scan across all legacy video files | Find matches between new video and legacy move clips. | Migration-5 |
| Migration-7 | Create draft move tracks for matches | Add appropriate move tracks into the video clip. | Migration-6 |
| Migration-8 | Apply inherited metadata as draft updates | Dancers, timing, and other metadata should update pending publish. | Migration-7 |
| Migration-9 | Replace legacy direct video link after publish | Do not replace or expose modern links until Publish to Moves is pressed. | Migration-7, MediaEdit-5 |

## Security And Admin

| ID | Action | Notes | Dependencies |
| --- | --- | --- | --- |
| Security-1 | Add mandatory login page | Required before accessing protected site content. | None |
| Security-2 | Add stay-logged-in behavior | Enabled by default. | Security-1 |
| Security-3 | Track IP visits | Store timestamped IP entries. | Security-1 |
| Security-4 | Track login events | Used for admin visibility. | Security-1 |
| Security-5 | Track content access | Track pages and videos viewed. | Security-1 |
| Admin-1 | Add admin page shell | Includes settings as a subpage. | Security-1 |
| Admin-2 | Add admin settings subpage | Houses security and site settings. | Admin-1 |
| Admin-3 | Show timestamped IP activity | Organize for quick scanning and comprehension. | Security-3, Admin-1 |
| Admin-4 | Show summarized access overview | Focus on pages and videos viewed. | Security-5, Admin-1 |
| Admin-5 | Show login activity summary | Include useful timestamps and account/IP relationships. | Security-4, Admin-1 |

## Recommended Implementation Sequence

### Phase 0: Architecture Audit

Start with the existing codebase and identify the actual shared components before building new UI. The highest-risk mistake would be building a second dropdown, grid, badge, or editor pattern instead of extending what already exists.

Actions:

- Infra-1
- Infra-2
- Infra-3
- Infra-4
- Infra-5
- Infra-6
- Infra-7

### Phase 1: Metadata Foundations

Topics, families, and dancers are foundational references for move editing, media filtering, clip naming, and migration metadata. These should be added before deeper move/media workflows.

Actions:

- Metadata-1 through Metadata-6
- Dancer-1 through Dancer-8

### Phase 2: Move Viewing And Editing

Once metadata exists, update the move pages and editing experience. This phase clarifies legacy versus modern moves and improves relationship workflows.

Actions:

- MoveView-1 through MoveView-5
- MoveEdit-1 through MoveEdit-12

### Phase 3: Media Filters And Publishing Contract

The publish-to-moves behavior is the central contract of the modern system. Build this before advanced video tooling or migration. Media edits should be allowed to exist as drafts until explicitly published.

Actions:

- MediaView-1 through MediaView-4
- MediaEdit-1 through MediaEdit-9
- MoveView-6
- MoveView-7

### Phase 4: Timeline, Crop, And Count Tools

After the publishing model works, add the advanced video editing features. This keeps the complex UI work grounded in a working media-to-move pipeline.

Actions:

- MediaEdit-10 through MediaEdit-26

### Phase 5: Migration

Build the migration process after modern clip sections, draft states, generated videos, and publish-to-moves exist. Migration should create draft modern links first, then apply them only on publish.

Actions:

- Migration-1 through Migration-9

### Phase 6: Security And Admin

Security can be moved earlier if the site is already exposed publicly. If this is still a private development workflow, it can come after the core content model. Login and tracking should not be bolted onto individual pages one by one; add middleware or route-level protection where possible.

Actions:

- Security-1 through Security-5
- Admin-1 through Admin-5

## Key Technical Contracts

### Publish To Moves

Media clips can be edited freely. Metadata and links are not pushed down to move pages until Publish to Moves is pressed.

Before publish:

- New move clips are unpublished.
- Changed published clips become draft.
- Draft clips show orange borders.
- Never-published clips show yellow borders.

On publish:

- Move pages receive clip links.
- Modern move metadata updates from the parent video/clip.
- Generated low-res padded and unpadded move videos are refreshed.
- Legacy direct links may be replaced only where the migration workflow explicitly allows it.

### Metadata Inheritance

Legacy moves hold their own stats.

Modern moves also store their own stats, but those stats are linked to their parent video/clip metadata and should stay updated through the publish workflow.

### Count Overlay

Count overlays are metadata-driven and generated live.

- They are previewed in the editor.
- They are not baked into videos.
- They are not enabled by default.
- They should behave like captions: available through a toggle.

### Migration Safety

Migration should never silently replace visible move content.

The scan process can detect matches and create draft move tracks, but replacing legacy direct video links and exposing modern links should happen only after Publish to Moves.

## Suggested First Implementation Slice

The best first slice is small but representative:

1. Build or confirm shared badge, picker, and grid infrastructure.
2. Add topic/family metadata CRUD under Create.
3. Add badges to move viewing.
4. Add topic/family dropdowns to move editing.
5. Add published/draft/unpublished status model for clips.
6. Add Publish to Moves as the first explicit media-to-move contract.

This slice exercises the main design direction without committing immediately to the harder video processing, count overlay, or migration work.
