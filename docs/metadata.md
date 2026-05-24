# Metadata Contracts

These contracts cover topic and family metadata.

Shared controls for metadata must follow `docs/shared-ui-audit.md`, especially searchable picker, editable list/grid, and filtered overview contracts.

## Create Area

- Topic and family management belongs under the Create area.
- The metadata area must handle both topics and families; do not split them into unrelated workflows unless the user explicitly asks for that.
- A topic has a name and description.
- A family has a name and description.
- Topic and family entries must be editable after creation.
- Topic and family entries must persist beyond the current browser session.
- Topic and family lists should use the shared editable list/grid pattern.

## Splash Pages

- Each topic must have a splash page.
- Each family must have a splash page.
- Topic splash pages must show the normal overview layout filtered to moves in that topic.
- Family splash pages must show the normal overview layout filtered to moves in that family.
- Topic and family splash pages should reuse the overview browsing model instead of becoming standalone marketing/detail pages.
