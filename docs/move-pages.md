# Move Page And Move Editing Contracts

These contracts cover move detail pages and move editing surfaces.

Shared controls on move pages and move editing surfaces must follow `docs/shared-ui-audit.md`, especially badge, searchable picker, status, and context menu contracts.

## Move Detail Page

- Move detail pages must place the move edit affordance inside the Move details panel as a compact wrench icon.
- Activating the move edit affordance must convert the Move details panel into an inline editor instead of navigating to a separate edit page.
- Video tab mouseover popovers must wait 1 second before appearing.
- Relationship fields should not be repeated as separate plain text fields when the relationship diagram can show them.
- The relationship diagram is the primary way to understand a move's parents, children, and related moves.
- Parent, child, and related relationships must be visually distinguishable in the diagram.
- If a move video came from an editable media clip, the move page must provide a way to go back to that clip editor.
- Move pages must show rendered media clips once per published clip; stale generated files and low-res variants must not appear as separate top-level videos.
- Move-page rendered clip tabs must show the source media timing/type/environment metadata rather than fallback `Other / Other / Class` bootstrap defaults.
- When a move has more than four linked videos, the move page must keep at most four videos in the main tab strip.
- When a move has more than four linked videos and no videos have been explicitly promoted, the first four videos in the existing move-video order are the default key videos.
- Additional non-key videos must remain accessible from the move page through a compact dropdown control instead of expanding the main tab strip.
- While the move page is in edit mode, an editable selected clip must expose a star toggle that promotes or demotes that clip as a key video.
- On a fresh site load, move-page videos must start unmuted.
- If the user manually mutes a video, that mute preference must be remembered across the site for up to 3 hours before videos default back to unmuted.

## Move Creation And Editing

- New move IDs must warn about collisions while the user is typing.
- The `ID` field shown in move creation/editing is the user-facing move ID, not the stable backend key for an already-published move.
- Published move edits must keep a stable backend key even when the user-facing move ID changes.
- Changing a published move's user-facing `ID` must not break backend move-to-video links or relationship references that use the stable backend key.
- Changing a published move's user-facing `ID` must rename existing rendered partnerwork clip filenames and poster sidecars to match the new visible ID.
- ID collision warning must happen before save or publish, not only as a failed submit.
- Topic selection must use searchable picking.
- Family selection must use searchable picking.
- Topic and family pickers must use the shared searchable picker behavior.
- Topic and family pickers must let the user commit a typed value when no existing option matches, so draft and published move edits can add new topics and families without leaving the editor.
- Adding a topic or family from move creation/editing must persist a metadata entry immediately so the value remains visible and selectable after reloads.
- Topic and family pickers in move creation/editing must not show visible `Change topic` or `Change family` placeholder text once a selected chip is already present.
- The move creation/editing workspace must use one visible title: `Draft move` for new or draft moves and `Edit move` for published moves.
- The move creation/editing workspace should avoid redundant `Details` and `Notes` section headings; field labels should carry those groups.
- The move creation/editing workspace should keep vertical spacing compact enough that Details, Notes, and Connections are visible without excessive scrolling.
- Move creation/editing must use a scoped editor visual system whose colors avoid dull flat-grey dominance.
- Move creation/editing font sizes must follow a consistent editor type scale across fields, labels, controls, relationship actions, and footer actions.
- Move creation/editing footer actions should place the available primary action first, then `Delete` when present, then `Mark for review`; `Delete` should read as a clear destructive action and `Publish`/`Save changes` should read as the primary action.
- The review flag action belongs in the move creation/editing footer, not in the sidebar; it should read as a lighter `Mark for review` button until pressed, then toggle to the darker marked-for-review state.
- The move creation sidebar must include a collapsible `Review` section for moves and drafts marked for review, using an arrow icon control rather than `Show`/`Hide` text.
- Move creation/editing buttons should look lively enough to read as deliberate controls, using saturated action colors, clear hover states, and restrained depth rather than flat dull fills.
- Move creation/editing footer action buttons should use solid fills, not gradients.
- Footer action hover states should animate smoothly and increase contrast without adding heavy blur.
- Mobile move creation/editing should use denser field chrome, a shorter title area, shorter empty note fields, and less card bulk than desktop.
- On mobile, the ID and Type controls should share one row evenly because both values are short.
- On mobile, Tags and Authorship should share one row.
- On mobile, the move card pane must not auto-hide, minimize, or become sticky; it should scroll away naturally as normal page content before the editor reaches the top of the viewport.
- The move creation/editing workspace should keep the Connections heading close to the relationship diagram.
- Move creation/editing section grouping may use subtle background treatment, but must not add extra vertical spacing.
- Move creation/editing placeholders should be short enough to avoid clipping in their fields.
- Positions and Authorship fields should not show placeholder text; their labels carry the meaning.
- Description and comments fields in move creation/editing must auto-expand while typing, stop at a reasonable maximum height, and then scroll internally instead of exposing a user resize handle.
- Description and comments fields should start compact because they auto-expand.
- Publishing a draft move must be disabled until the draft has the minimum required fields: name, ID, and no ID collision.
- Draft and new move edits must autosave after changes instead of exposing a `Save draft` or `Save draft changes` button.
- Saving move drafts and published move edits must persist to the move edit store even if the existing store file is read-only but the data directory is writable.
- Saved drafts in the move creation workspace must expose a delete draft action.
- Published moves must allow editing the user-facing `ID` field from the move creation/editing workspace.
- The move level control must be a compact-width dropdown tucked into the same row as the move name, and only the level control should be height-adjusted to match neighboring identity fields.
- In the regular move identity row, ID must appear before Level.
- Compact Level and Type controls should feel visually related while preserving their positions in the form.
- Relation type segmented controls must show unselected options on a soft off-white background with a light border, and the selected option in cyan blue.
- The blank move level option must display as `—`.
- Fields in the move identity row must align from the top so compact controls do not sit lower than neighboring fields.
- The Details field order must place relation type before topic and family.
- The persisted source field must be labelled `Authorship` in move creation/editing UI.
- Relation type must stay as explicit segmented controls in the regular layout, then switch to a compact dropdown styled like a badge with an arrow in explicitly compact layouts or whenever the segmented options are measured not to fit without clipping or overlap.
- The compact relation type dropdown must be operable when clicked and must update the selected relation type.
- The compact relation type dropdown and its opened menu must use the same visual style as the segmented relation type buttons.
- Relation type options must be ordered `Base`, `Addition`, `Variation`; `Base` is the default and persists as the empty type value.
- Relation type should not be a freeform text field.
- Move relationship picker dropdowns in the creation/editing workspace must not be clipped by the editor card, relationship panels, or scrollable relationship diagram viewport.
- Relationship diagram move nodes should show a hover preview instead of the native SVG tooltip; nodes with a playable move video must include an unpadded sub-clip video plus the move name and ID, and nodes without video must still show a compact name and ID preview.

## Pinned Moves

- Pinned moves must be removable with a compact remove affordance beside the pinned move.
- Visible pinned/left-pane moves must support context menu relationship actions.
- Relationship context menus must support right-click.
- Relationship context menus must support long-press for touch use.
- Relationship context menus must follow the shared context menu behavior.
- Relationship context menu actions must include `add as parent`, `add as child`, and `add as related`.
- Context menu relationship actions apply to the move currently being edited.
- Move creation/editing connection controls must use the move relationship diagram instead of separate parent, child, and related picker panels.
- Move creation/editing connection controls must provide one `Add` button inside the top-left of the relationship diagram whose dropdown offers `Parent`, `Child`, and `Related`.
- The in-diagram `Add` button must be visually distinct from move nodes so it reads as a control, not as part of the graph.
- Empty connection diagrams should collapse to a compact empty state with the add controls and no graph counts.
- Empty connection diagrams should not show `No relationships yet` helper text.
- Relationship diagram zoom controls should sit inside the top-right of the diagram viewport.
- Add relationship menu choices should be direct actions: `Add parent`, `Add child`, and `Add related`.
- The add relationship control must remain a dropdown; in the empty Connections state it should open as a compact vertical dropdown attached to the triggering button with a clear cyan divider, matching trigger/row heights, top corners matching the closed pill while bottom corners flatten into the dropdown join, a visible slightly slower smooth eased height-reveal animation, normal editor field-scale option text, stay inside the Connections card instead of overlapping nearby form controls, and in all states it must render over editor card boundaries instead of being clipped.
- Choosing an add relationship action should open the move picker inside the same `Add` dropdown, not as a persistent separate field in the diagram toolbar.
- The add relationship dropdown must close when a move is selected, when Escape is pressed, or when the user clicks outside the dropdown.
- Move creation/editing relationship diagram nodes for connected moves must provide compact `x` remove controls.

## Review Flags

- Move content must support a review flag.
- Review flags must persist.
- Review notes must persist with the flag.
- Review controls should be hidden by default.
- When a move is marked for review from the footer, review notes should appear inline under the footer action buttons instead of in the sidebar, with the `Review notes` label italicized.
- If content is already flagged, the review section should be easy to reveal and inspect.
