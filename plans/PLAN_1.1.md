# Plan 1.1_finished: Complete Partially Implemented Plan 1.0 Work

Plan 1.1 is only for fixing work from Plan 1.0 that has already been started but is not fully correct, complete, or consistently applied.

This plan does not include fully finished work. It also does not include untouched future work such as migration matching, security, login, access tracking, or admin activity pages.

## Actions

- `Infra-5`: Finish shared editable-list adoption. `EditableList.svelte` exists, but routes still mostly hand-roll editable list/grid markup with shared CSS classes instead of using the shared component.

- `MoveView-3`: Tighten move detail badges. Move pages show badges, but timing, type, environment, legacy status, and modern status are not yet represented as one clean and consistent badge model across relevant move and video states.

- `MoveView-4` and `MoveView-5`: Clarify legacy-versus-modern metadata behavior. The video model has legacy and modern concepts, but the inheritance contract is not yet obvious in the move model or UI. Modern metadata should read as linked to parent media and controlled by publish-to-moves.

- `MoveView-6`: Expose generated low-res variants. Low-res and padded low-res references exist in data and rendering, but move pages still play the main file rather than exposing or choosing those generated variants where useful.

- `MoveEdit-5`: Replace remaining freeform structured fields. Some move fields already use structured controls, but `positions`, `tags`, and `source` are still freeform. Where stable option sets exist, use controls that match the rest of the editor.

- `Dancer-5`: Finish dancer region editing. Region is currently a plain input. It should offer existing region values while still allowing custom text.

- `MediaView-3`: Make dancer filtering complete. Dancer filtering exists, but appears to operate over currently loaded client-side assets. It should work reliably across the full media library, either through server-backed filtering or full option/data loading.

- `MediaEdit-1`: Finish clip deep-link behavior. Deep-linking to a clip exists, but it selects the clip rather than clearly opening the intended edit context. A move-page `Go to clip` link should land the user in the correct media editor state for editing that clip.

- `MediaEdit-7`: Fix changed-unpublished detection. Changed/unpublished state exists, but crop, count, and other non-range clip edits may not update server-side `updatedAt`, so published clips can fail to appear as changed drafts.

- `MediaEdit-13` and `MediaEdit-14`: Complete crop behavior. Crop can be drawn and stored, but rendered move videos do not appear to apply the saved crop metadata yet.

- `MediaEdit-20`: Replace count placement dropdown with direct corner controls. Count marker placement exists, but as a dropdown instead of four explicit corner controls.
