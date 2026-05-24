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
- Draft moves must be accepted by the move editor wherever published moves are accepted for clip definitions.
- Existing draft moves should be loaded into the media editor as valid searchable move options.
- Draft moves remain editable later on the Create move page.

## Editing Layout

Entering move-editing mode should make the video easier to inspect.

- When move editing is enabled, the video area should enlarge.
- The enlargement should be animated smoothly.
- The side metadata panel may collapse while editing to give the video more room.
- The layout must remain usable on mobile; the video/editor must not overflow horizontally or push controls off-screen.
- Mobile layout should prefer a single-column editor with compact controls.

## Video Playback

The editor should keep playback predictable while the user marks precise ranges.

- The source video may autoplay muted when loaded, subject to browser autoplay rules.
- Spacebar toggles playback only when focus is not inside an input, textarea, select, button, or editable element.
- Clicking the video toggles playback.
- Move-editor controls must not duplicate a large center play overlay and a bottom-right play button.
- Playback controls should be consolidated into one compact control group near the timeline metadata, next to the left of the `Total` element.
- The play/pause icon should be small.
- The old bottom-right play button should not be shown.

## Audio And Muting

Audio should be helpful when editing moves, but should respect the user.

- Track whether the user manually muted the video in the current browser session.
- If the user has not manually muted, enabling move editing should turn audio on.
- If the user has manually muted, enabling move editing must not unmute the video.
- Setting volume to zero counts as a manual mute.
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
- Saving clip definitions may target draft move IDs.
- Rendering and publishing behavior can still depend on later completion of the draft move workflow, but the editor must not block clip definition work just because a move is still a draft.

## Timeline Zoom

- The timeline must support zooming in and out.
- When timeline zoom is active, the editor must clearly show that zoom is active.
- When timeline zoom is active, the editor must provide a reset zoom control.

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
