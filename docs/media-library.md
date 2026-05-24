# Media Library Contracts

These contracts cover the media browsing page.

Shared controls in the media library must follow `docs/shared-ui-audit.md`, especially badge, searchable picker, and status contracts.

## Filters

- Media browsing must support publication-state filtering.
- Publication-state filter options must include published, unpublished, and draft.
- Media browsing must support filtering by social/class type.
- Media browsing must support filtering by dancer.
- Dancer filtering should use the shared searchable picker behavior.
- Dancer filtering is not complete until it works reliably across the full media library, not only currently loaded client-side assets.

## Status Display

- Published media-linked content should look stable and neutral.
- Changed-but-unpublished clips must be marked as draft.
- Changed-but-unpublished clips must use an orange draft indicator.
- Never-published move clips must be visually distinct from published content.
- Never-published move clips must use a yellow indicator.
- Publication, draft, processing, and legacy/modern status display must use the shared status language.
