# Move Editor Behavior Rules

This page captures the product rules for the source-video move editor at:

```text
/media/edit/[source-asset-id]
```

The editor is used to mark move ranges inside a source video, save clip definitions, and render playable move clips. These rules should guide future changes to the move editor UI and related APIs.

Shared controls in the move editor must follow `docs/shared-ui-audit.md`, especially searchable picker, badge/status, and context menu contracts.

## Move Search

The move search field must support fast inline editing while a user is working through a source video.

- Search must match by move ID and move name.
- Inline move-search dropdowns must not be clipped by the surrounding editor row or timeline container.
- When the picker is embedded inside the move row, the dropdown should float relative to the viewport so it remains usable on desktop and mobile.
- The dropdown must include a `Create new move` entry at the top when the user has typed a non-empty query.
- The create entry must use the typed query as the draft move name.
- Pressing Enter should still choose the best existing match when matches exist. The create entry remains available with keyboard arrows or pointer/touch.

## Draft Moves

Creating a move from the move-search dropdown must keep the user in the media editor.

- Do not jump the user to the Create move page.
- Create a saved draft move immediately.
- Generate a stable draft move ID from the typed name, avoiding collisions with published moves and existing drafts.
- Add the newly created draft move to the current move row immediately.
- Draft move chips in the media editor must show `[Draft]` in place of the generated draft move ID.
- Draft moves must be accepted by the move editor wherever published moves are accepted for clip definitions.
- Existing draft moves should be loaded into the media editor as valid searchable move options.
- Draft moves remain editable later on the Create move page.
- If a draft move's ID or display ID changes before publication, any saved or rendered media clips already attached to the previous draft move ID must be relinked immediately.
- When a draft move that is already attached to saved media clips is published, those clips must be relinked to the published move ID and display ID automatically.
- After that publish, the media editor must continue to show the clip's move ID and move name without requiring the user to recreate or reattach the clip.

## Editing Layout

Entering move-editing mode should preserve the user's spatial context while enabling precise clip controls.

- When move editing is enabled, the video player must stay at its current layout size.
- Edit mode must not expand the player, collapse side panels, or change the media editor grid just to provide more video room.
- The media editor source video player should use a compact height cap that is 15% shorter than the previous `min(56vh, 560px)` cap.
- Edit mode must keep the timeline and clip controls visible without the video dominating the viewport.
- The media editor must not show a `Back to media` button above or inside the editor chrome, so navigation does not consume editor height or header space.
- The side metadata card must remain fully inside the viewport; its rounded corners and edit control must not be clipped by horizontal overflow.
- The layout must remain usable on mobile; the video/editor must not overflow horizontally or push controls off-screen.
- Mobile layout should prefer a single-column editor with compact controls.
- The timeline must never paint past the video/editor column into the side metadata area or past the window edge.
- When there is enough inline space for the video/timeline column and the side metadata card, the metadata card must stay beside the video/timeline instead of moving above it.
- When the editor does not have enough inline space for both the video/timeline column and the side metadata card, the metadata card must move above the video/timeline instead of remaining as a squeezed side column.

## Video Playback

The editor should keep playback predictable while the user marks precise ranges.

- The source video should default to unmuted on a fresh site load, while any autoplay attempt still remains subject to browser autoplay rules.
- Spacebar toggles playback only when focus is not inside an input, textarea, select, button, or editable element.
- Clicking the video toggles playback.
- Move-editor controls must not duplicate a large center play overlay and a bottom-right play button.
- Playback controls should be consolidated into one compact control group near the timeline metadata, next to the left of the `Total` element.
- The compact playback control group should look polished and intentional, with clear icon buttons, readable focus/hover states, and a refined inline volume slider.
- The source-video fullscreen control should live in the compact playback control group and must not resize the editor grid, expand the player in-page, or consume additional timeline height.
- Source-video playback controls and move-context boxes should stay vertically compact so they do not consume unnecessary timeline height.
- Playback move-context boxes must stay in stable left, center, and right slots at every viewport width; they must never stack vertically.
- The play/pause icon should be small.
- The old bottom-right play button should not be shown.

## Audio And Muting

Audio should be helpful when editing moves, but should respect the user.

- Track whether the user manually muted any site video.
- If the user manually muted a video, that mute preference must be remembered across the site for up to 3 hours.
- If no active mute preference exists, enabling move editing should leave audio on.
- If an active mute preference exists, enabling move editing must not unmute the video.
- Setting volume to zero counts as a manual mute.
- The source-video mute button must directly toggle muted/unmuted audio and persist the same site-wide mute preference as native video controls.
- Opening a mobile volume slider must not replace the mute button's toggle behavior; use a separate compact control when the slider needs its own opener.
- On mobile, the volume control should be compact: a button opens a small dropdown/popup volume slider.
- On desktop, the volume slider may be visible inline inside the compact control group.

## Trim Marker Dragging

Dragging trim or move markers is a precision-editing interaction and must not unexpectedly start playback.

- When the user starts dragging a trim or move marker, the video should show the paused frame for the marker position.
- Dragging a marker must not start playback.
- While a marker is being dragged, do not show the play icon.
- If the video was already playing before the marker drag began, pause it during the drag and resume playback after the marker is dropped.
- If the video was paused before the marker drag began, keep it paused after the marker is dropped.
- When a marker is dropped, seek back to the move start preview position before resuming if resuming is required.
- Dragging the playhead is separate from dragging trim/move markers and should remain a direct seek interaction.

## Saving Clips

The move editor saves clip definitions before rendering.

- Clip rows are saveable when they contain at least one valid published move or draft move and the row range has positive duration.
- Each visible clip row represents exactly one move expression.
- Each clip row must allow only one move selection at a time; adding another move expression requires a separate row.
- A clip row may store an optional extra move label, optional start position, and optional end position.
- Start and end positions must come from the strict position picker option set; the media editor must not save arbitrary position text.
- The optional extra move label describes only that clip expression and must not rename the canonical move.
- Multiple move expressions that share the same clip/action timing must be represented as separate rows bound by a shared timing group.
- Bound rows must keep locked same-time behavior: editing timing on one row updates the other rows in the group.
- Bound rows must share a gently contrasting background so the shared timing relationship is visible.
- Saving clip definitions may target draft move IDs.
- Rendering and publishing behavior can still depend on later completion of the draft move workflow, but the editor must not block clip definition work just because a move is still a draft.
- Saved clips must remain visible in the timeline after they are created, in the same lower timeline lane where move ranges appear during editing.
- Saved clip ranges and other compact lower-lane move ranges must sit below the playhead handle instead of overlapping the playback track or playhead marker, in both playback and edit modes.
- The saved clip range containing the current playback position must use a lighter green than other saved clip ranges.
- The playhead handle circle must exactly fill the vertical space between the playback track bottom and the lower clip lane top, based on measured track and lane geometry rather than a guessed size.
- Outside active move editing, saved clips must be positioned against the full source-video timeline unless the user has intentionally zoomed the timeline.
- Entering move-editing mode from a source video must not create a new blank row or preselect an existing saved clip. It should open the editor surface with saved clips visible and wait for the user to explicitly click `Add move` or select a saved clip row/timeline range.
- While the move editor is open, previously saved clips must also appear as compact rows in the editor form so they can be selected for editing from the same surface as new draft rows.
- While the move editor is open, the row list should show only the currently clicked move plus the three other move rows closest to the video playhead, rather than every saved move row in the source video.
- While the move editor is open, green saved clip bars in the lower timeline lane must continue to show all eligible saved clips, not only the filtered row-window clips.
- Selecting or editing a clip must not reorder the clip rows; active draft rows should replace their saved row in place.
- Saved clip rows must use the same Start, Move, Label, Start position, and End position columns as editable rows, even when older clips do not yet have descriptor or position values.
- The move clip row area in edit mode must use a fixed five-row viewport by default with its own vertical scroll when more rows exist, so adding saved or draft move rows does not keep pushing the player/timeline layout taller.
- Selecting a saved clip or draft move that is outside the current five-row viewport must automatically scroll the row viewport just enough to reveal the selected row.
- Draft and saved move rows must keep their columns aligned at desktop and tablet widths; if the viewport cannot fit the full row, the row area should scroll horizontally or use a deliberate narrow layout rather than allowing header labels and row controls to auto-place unpredictably.
- In active edit mode, the video/timeline column must either reserve enough width for the fixed editor row grid or stack separately from the metadata panel; timeline and row controls must not render underneath the metadata panel.
- New move clips must default to key/prime while the target move has fewer than four clips; once the target move has four clips, additional new clips default to non-key/non-prime.
- Saved clip rows, including the active edited saved row, must provide the star toggle for marking that clip as a key video; the editor must not show a separate top-level toolbar star button.
- Saved clip rows must provide a visible delete action for removing a move clip.
- Deleting a move clip from the editor marks it for removal; the deletion is persisted only after saving clip changes.
- Creating or editing a clip must not turn existing clips into a large alternate representation; new or edited clips should receive only a small unsaved marker until the user saves the clip changes.
- When editing an existing saved clip, its old saved timeline range must be hidden; the active draft range is the clip's temporary timeline representation until saved.
- The editor must not show a separate `Move clips` card/list for saved clips; the timeline is the clip list.

## Timeline Zoom

- The timeline must support zooming in and out.
- Timeline zoom must be available in playback mode and move-editing mode.
- Scrolling over the timeline should zoom around the pointer position.
- Pinching on the timeline should zoom around the pinch midpoint.
- Scroll and pinch zoom should use smooth, responsive sensitivity so gestures feel direct without stepping or jumping.
- Wheel zoom should accumulate into a smooth target viewport and ease continuously toward it rather than stepping/chunking per wheel event.
- Timeline geometry should begin animating immediately during zoom gestures, using a short direct transition rather than the slower edit-row motion pace.
- When timeline zoom is active, the editor must clearly show that zoom is active.
- Timeline zoom affordances, including the `Zoomed` status, overview strip, edge arrows, and `Reset zoom` / `Loop zoom` controls, should fade or gently animate in and out instead of popping abruptly.
- The main timeline action row should animate button reflow when zoom controls appear or disappear, so existing actions glide into their new positions instead of jumping.
- The zoomed timeline boundary lines must show compact left and right arrow indicators centered 15px above the respective orange edges, not attached to the side markers.
- Mobile touch pinch zoom should be 10x more sensitive than the base pinch damping while mouse wheel zoom sensitivity remains unchanged.
- When timeline zoom is active, the editor must provide `Reset zoom` and `Loop zoom` controls in the main timeline action row beside `Edit moves` or `Add move`; zoom controls should be hidden when no zoom window exists.
- Zoom controls must remain visibly grouped with the main timeline action on narrow screens rather than wrapping or disappearing out of view.
- `Loop zoom` must loop playback over the visible zoom window and remain mutually exclusive with clip-range looping.
- Timeline tracks, saved ranges, playhead lines, knobs, and drag markers must render crisply at rest; the editor must not rely on transform downscaling or fractional sizing that leaves the timeline persistently blurry.
- Saved clip range markers in the timeline should not use visible border or outline rings, and their corners should stay modestly rounded rather than pill-shaped.
- The timeline play tracker handle should be 4px shorter vertically than its original square size while staying centered on the same vertical midpoint.
- Timeline and edit-row animations must be smooth, restrained, and visibly paced rather than snappy; direct marker/playhead dragging must remain immediate without trailing transitions, and reduced-motion preferences must disable nonessential motion.
- Playback-nearest edit-row updates must settle briefly before changing the visible row window, so ordinary playback does not constantly reshuffle rows; when the settled window changes, rows should slide smoothly rather than jump.
- Clicking `Edit` on a different saved or draft row must keep the underlying row identity stable so the row window slides/reflows smoothly instead of tearing down and rebuilding rows.
- When a saved green move range is opened for editing, it should lift from the lower lane into the active timeline lane while turning orange; the clip padding range and trim/move markers must fade in only after that lift completes.

## Crop Tool

- The editor must support a per-move crop tool.
- The user must be able to draw a crop rectangle for a generated move video.
- Crop metadata must be saved with the clip.
- Crop behavior is not complete until saved crop metadata affects generated move videos.

## Count Placement

- The editor must provide a count-placement mode for clips.
- Count-placement mode must zoom the timeline to the current move with padding.
- The default on2 count sequence is `6, 7, 1, 2, 3, 5`.
- The user must be able to move the video player, place the current count, and advance to the next count.
- Count placement must support a finish action.
- Count marker placement should use direct corner controls.
- Timing presets must include `on2`, `on2 all counts`, `on1`, and `on1 all counts`.
- `on2 all counts` must add `4` and `8` to the default on2 sequence.
- On1 count sequences must support `1, 2, 3, 5, 6, 7` and an all-count equivalent.
- Count overlays must support clear flags so a count can hold until the next count or clear.
- Count overlays must be previewable live in the editor.

## Runtime Count Overlays

- Runtime count overlays must be available through a user-controlled toggle.
- Runtime count overlays must not be enabled by default.

## Mobile Expectations

The mobile editor should preserve the same workflow with denser controls.

- Controls should be compact enough for a phone viewport.
- The volume slider should not take permanent horizontal space on mobile.
- Timeline metadata, playback controls, and total duration should sit together without causing horizontal overflow.
- Text in buttons and controls should not overlap or force layout jumps.
