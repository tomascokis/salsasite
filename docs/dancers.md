# Dancer Contracts

These contracts cover dancer profiles and dancer browsing.

Shared controls on dancer pages must follow `docs/shared-ui-audit.md`, especially searchable picker and editable list/grid contracts.

## Navigation And Profile Shape

- Dancers must be a first-class site area in the main navigation.
- A dancer profile must support full name.
- A dancer profile must support display name.
- A dancer profile must support Instagram handle.
- A dancer profile must support role values: `lead`, `follow`, and `unknown`.
- A dancer profile must support level values: `world class`, `pro`, `semi-pro`, `amateur`, and `unknown`.
- Dancer profiles must support region. Region behavior is not complete until it offers existing regions while still allowing custom text.
- Region entry should use the shared searchable picker pattern once region options exist.

## Viewing And Editing

- Dancer profiles should be viewable by default.
- Editing should be entered through a small, unobtrusive edit affordance.
- Dancer list/profile editing should use the shared editable list/grid pattern where practical.
- The profile view must show dances associated with the dancer.
- The profile view must show moves associated with the dancer.
- A dancer profile must be deletable from the profile workflow. Deleting a profile removes the stored dancer row when one exists and suppresses derived profiles with the same dancer slug without deleting associated media, move records, or source metadata.
