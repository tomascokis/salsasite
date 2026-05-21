# Editing Interface UI Action Plan

This plan supersedes the broader UI review. The overview page is explicitly out of scope.

## Scope Guardrails

- Do not change `/` or the overview dashboard behavior, layout, density, colors, row styles, or search UI.
- Avoid broad CSS changes that can affect overview classes such as `.dashboard-page`, `.dashboard-columns`, `.dashboard-row`, `.title-row`, `.data-row`, `.level-cell`, and `.name-cell`.
- Prefer editor-scoped selectors such as `.media-editor-page`, `.move-editor-page`, `.move-create-page`, `.metadata-layout`, `.upload-editor-shell`, `.media-clips-card`, and editor-specific component classes.
- Any shared button/form/card primitives must be opt-in classes, not blanket replacements for all existing buttons or cards.

## Current Diagnosis

The editing surfaces are powerful but visually uneven. Media editing has a strong video/timeline workspace, but review mode and edit mode compete for space. Move tracks need to feel more like editable records. Move editing and metadata creation use several card, form, and button patterns that do not fully align with the media editor. Crop/count tools need clearer mode boundaries so the user always knows what clicking the video will do.

Primary goals:

- Make editing workflows tighter and more operational.
- Keep the primary media/video context visible while editing.
- Make save, publish, delete, crop, count, and edit states unambiguous.
- Normalize editor-only buttons, forms, cards, toolbars, and status messages.
- Improve desktop and mobile editor ergonomics without touching overview.

## Phase EDIT-0: Editor-Scoped Visual System

### EDIT-0.1 Add Editor-Only Tokens

Define editor-scoped CSS variables on editor root classes rather than globally changing the whole app.

Implementation notes:

- Add variables under `.media-editor-page`, `.move-editor-page`, `.move-create-page`, and `.metadata-layout`.
- Include compact spacing, control height, surface radius, subtle border, selected state, danger state, and focus ring tokens.
- Keep default editor surfaces at 8px radius unless the media frame itself needs stronger clipping.

Acceptance criteria:

- Editor pages share spacing and control feel.
- Overview/dashboard classes are untouched.

### EDIT-0.2 Create Opt-In Editor Button Variants

Introduce editor-only button variants.

Implementation notes:

- Add classes such as `.editor-button`, `.editor-button-primary`, `.editor-button-secondary`, `.editor-button-danger`, `.editor-button-ghost`, `.editor-icon-button`.
- Apply them only inside editing routes/components.
- Convert delete controls to danger styling, edit controls to icon or ghost styling, and save/publish controls to primary styling.

Acceptance criteria:

- Save/publish/delete/edit actions are visually consistent across media edit and move edit.
- Destructive actions are immediately distinguishable.

### EDIT-0.3 Normalize Editor Forms

Create editor-scoped input/select/textarea styling.

Implementation notes:

- Use `.editor-field`, `.editor-label`, or route-scoped selectors.
- Normalize compact padding, line-height, focus ring, disabled state, and validation/status text.
- Apply to media properties, move editor fields, metadata creation, move picker, and clip name fields where practical.

Acceptance criteria:

- Inputs in media edit and move edit feel like the same editing system.

## Phase EDIT-1: Media Editing Layout

### EDIT-1.1 Split Review Mode From Edit Mode

The media editor should not always show a full editing timeline footprint.

Implementation notes:

- Default/review mode: show video, compact scrubber, source metadata, and primary actions.
- Edit mode: expand timeline, move drafting controls, clip tools, and track list.
- Use a clear mode toggle such as `Review` / `Edit moves`.

Acceptance criteria:

- At 1280x720, default media editor view shows video, metadata, and primary actions above the fold.
- Full timeline controls appear only when they are useful.

### EDIT-1.2 Rebalance Video And Metadata Columns

The video surface should remain central, but metadata/actions should not feel detached.

Implementation notes:

- Keep source metadata as a compact side panel on desktop.
- On medium widths, place metadata in a compact strip above or below the video.
- Reduce unnecessary card shadows and large rounded corners around the video workspace.

Acceptance criteria:

- The editor no longer reads as one huge video card plus a separate metadata card.
- The first viewport communicates what asset is being edited and what actions are available.

### EDIT-1.3 Compact The Timeline

The dark timeline/control area is visually heavy.

Implementation notes:

- Collapse total duration, scrubber, playback, and edit button into a tighter control bar in review mode.
- In edit mode, keep the larger timeline but reduce vertical padding and redundant labels.
- Keep timeline markers visually clear, but reduce glow/shadow effects.

Acceptance criteria:

- Timeline controls are scannable and do not dominate the video.

## Phase EDIT-2: Move Track Editing

### EDIT-2.1 Make Track Rows Table-Like

Move tracks should read like editable records, not loose cards.

Implementation notes:

- Desktop layout: time, move ID/name, publication/render status, actions.
- Use fixed action column width so row controls do not shift.
- Keep compact status badges, but reduce badge visual weight.

Acceptance criteria:

- A user can scan all tracks quickly and see which ones need saving, rendering, or publishing.

### EDIT-2.2 Improve Track Actions

Track actions need a clear hierarchy.

Implementation notes:

- Primary row action: select/edit track.
- Secondary actions: duplicate or rename if present later.
- Danger action: delete, using the editor danger style.
- Keep the last-track empty state and save action visible.

Acceptance criteria:

- Deleting a track is clear, reversible until save, and visually distinct.

### EDIT-2.3 Clarify Track Persistence

Make unsaved track changes obvious.

Implementation notes:

- Add a compact dirty-state indicator when track rows differ from saved state.
- Use one consistent save status area.
- Disable or visually de-emphasize publish controls when unsaved changes exist.

Acceptance criteria:

- Users know whether changes are local-only, saved, rendered, or published.

## Phase EDIT-3: Crop And Count Tools

### EDIT-3.1 Enforce Exclusive Edit Modes

Crop, count placement, and normal playback should be mutually exclusive.

Implementation notes:

- Switching to crop exits count mode.
- Switching to count placement exits crop mode.
- Returning to playback clears placement/crop interaction state without deleting saved data.

Acceptance criteria:

- Clicking the video always has one obvious meaning.

### EDIT-3.2 Add A Compact Mode Toolbar

When a clip is selected, show clip tools as a compact toolbar close to the video.

Implementation notes:

- Controls: Crop, Clear crop, Place counts, Finish, Clear counts, count preset, count placement.
- Use selected/active state from editor button variants.
- Keep advanced count details below the toolbar only when count mode is active.

Acceptance criteria:

- Clip tools are visible where the user is working, not buried below the track list.

### EDIT-3.3 Improve Count Marker Feedback

The count placement flow needs clearer status.

Implementation notes:

- Show current marker, step count, and timestamp in one compact inline status.
- Make placed markers visually distinct from pending markers.
- Add short status copy only when it changes state; avoid persistent instructional text.

Acceptance criteria:

- Users can place counts without guessing which marker comes next.

## Phase EDIT-4: Move Editor And Metadata Editor

### EDIT-4.1 Tighten Move Editor Page Structure

Move editing should feel like a focused form/workbench.

Implementation notes:

- Reduce oversized hero/card treatment.
- Use compact section headers and tighter form grids.
- Keep save/cancel actions sticky or consistently available near the top/bottom.

Acceptance criteria:

- Editing a move feels faster and less page-like.

### EDIT-4.2 Normalize Metadata Creation

Metadata create/edit pages should share the same editor form system.

Implementation notes:

- Align field sizes, labels, section spacing, and segmented controls with media edit.
- Make required/optional fields visually clear without heavy helper text.
- Keep validation/status messages in a single predictable area.

Acceptance criteria:

- Creating or editing metadata feels consistent with editing media and moves.

### EDIT-4.3 Improve Relationship Editing Surfaces

Relationship/component editors should be compact and explicit.

Implementation notes:

- Use table/list rows with clear add/remove actions.
- Avoid nested card surfaces inside editor sections.
- Use compact chips for selected moves/components.

Acceptance criteria:

- Relationship editing reads as structured data entry, not scattered cards.

## Phase EDIT-5: Editor Navigation And Status

### EDIT-5.1 Standardize Editor Top Bars

Every editing screen needs a predictable top action zone.

Implementation notes:

- Left: back link and entity name.
- Middle or below: edit mode/status.
- Right: save, publish/render, delete, or secondary actions.
- Keep route-level app nav unchanged except where absolutely necessary.

Acceptance criteria:

- Users can always find save and back actions without scanning the page.

### EDIT-5.2 Unify Status Messages

Status messages currently appear in several places.

Implementation notes:

- Standardize success, pending, warning, and error message placement.
- Use compact inline status near the related action when possible.
- Use persistent page-level status only for broad save/publish/render results.

Acceptance criteria:

- Users can tell whether an action succeeded and what to do next.

## Phase EDIT-6: Responsive Editing QA

### EDIT-6.1 Mobile Editing Order

Define route-specific mobile order for editing surfaces.

Implementation notes:

- Media editor review mode: source identity/actions, video, compact metadata, track summary.
- Media editor edit mode: video, mode toolbar, timeline, track rows, metadata.
- Move editor: identity/actions, required fields, relationship fields, optional notes/media.

Acceptance criteria:

- No editing page requires accidental horizontal scrolling below 920px.
- Primary actions remain reachable.

### EDIT-6.2 Text Fit And Touch Targets

Run visual QA on long filenames, long move names, dense buttons, and status badges.

Acceptance criteria:

- Long names truncate or wrap intentionally.
- Editor buttons do not overflow.
- Touch targets are at least 36px high on mobile editor screens.

## Phase EDIT-7: Preview-Based QA

Review only editing-related pages at desktop and mobile widths:

- `/media/edit/0f6a3b04-aff4-4fc8-9208-9de75d04d923`
- `/moves/WRP00002/edit`
- `/moves/create`
- `/moves/create/metadata`
- Any metadata/edit route affected by shared editor CSS

Acceptance criteria:

- Overview page is unchanged.
- Editor pages have consistent controls, spacing, action hierarchy, and mode states.
- No nested-card visual stacks in editor layouts.
- Save/delete/publish/render/crop/count flows remain functional.

## Suggested Execution Order

1. `EDIT-0`: Add editor-scoped tokens, buttons, and form primitives.
2. `EDIT-1`: Rebalance the media editing layout and timeline density.
3. `EDIT-2`: Tighten move track rows and persistence status.
4. `EDIT-3`: Clarify crop/count mode behavior and toolbar placement.
5. `EDIT-4`: Bring move editing and metadata editing into the same editor system.
6. `EDIT-5`: Standardize editor top bars and status messages.
7. `EDIT-6` and `EDIT-7`: Responsive and preview QA, with overview unchanged.
