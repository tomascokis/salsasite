# Move Page And Move Editing Contracts

These contracts cover move detail pages and move editing surfaces.

Shared controls on move pages and move editing surfaces must follow `docs/shared-ui-audit.md`, especially badge, searchable picker, status, and context menu contracts.

## Move Detail Page

- Relationship fields should not be repeated as separate plain text fields when the relationship diagram can show them.
- The relationship diagram is the primary way to understand a move's parents, children, and related moves.
- Parent, child, and related relationships must be visually distinguishable in the diagram.
- If a move video came from an editable media clip, the move page must provide a way to go back to that clip editor.

## Move Creation And Editing

- New move IDs must warn about collisions while the user is typing.
- ID collision warning must happen before save or publish, not only as a failed submit.
- Topic selection must use searchable picking.
- Family selection must use searchable picking.
- Topic and family pickers must use the shared searchable picker behavior.
- Relation type must be selected with explicit controls for `Addition`, `Variation`, or neither.
- Relation type should not be a freeform text field.

## Pinned Moves

- Pinned moves must be removable with a compact remove affordance beside the pinned move.
- Visible pinned/left-pane moves must support context menu relationship actions.
- Relationship context menus must support right-click.
- Relationship context menus must support long-press for touch use.
- Relationship context menus must follow the shared context menu behavior.
- Relationship context menu actions must include `add as parent`, `add as child`, and `add as related`.
- Context menu relationship actions apply to the move currently being edited.

## Review Flags

- Move content must support a review flag.
- Review flags must persist.
- Review notes must persist with the flag.
- Review controls should be hidden by default.
- If content is already flagged, the review section should be easy to reveal and inspect.
