# Frontend App

This is now a real `SvelteKit` application scaffold intended to run inside Docker on Unraid.
The preferred deployment is a live-mounted container, where the repo is bind-mounted and the image only provides the runtime/dependencies.

## Implemented routes

- `/`
  Multi-column move dashboard plus search
- `/moves/[slug]`
  Move detail page using runtime video-library links from `video-moves/` and `video-sources/`
- `/progress`
  Historical progress snapshots rendered from exported JSON
- `/media`
  Source-video media library, metadata editing, multi-move linking, and clip slicing
- `/upload`
  Compatibility redirect to `/media`
- `/progress/editor`
  Browser-side editing shell with CSV import/export and keyboard shortcuts
- `/api/search`
  Search endpoint backed by `migration-data/search-index.json`
- `/media/[...path]`
  Local media streaming route with byte-range support for videos

## Runtime data

The app reads from:

- `migration-data/manifest.json`
- `migration-data/moves.json`
- `migration-data/layout.json`
- `migration-data/progress.json`
- `migration-data/search-index.json`
- `migration-data/raw-moves.json`
- `migration-data/raw-moves-schema.json`

At runtime the app expects:

- `DATA_DIR`
  Defaults to `../migration-data`
- `MEDIA_ROOT`
  Defaults to `../video-moves`
- `SOURCE_ROOT`
  Defaults to `../video-sources`
- `POSTER_ROOT`
  Defaults to `../video-posters`
- `POSTER_TIMESTAMP_SECONDS`
  Defaults to `1.0` and controls which frame is used when the server auto-generates a missing poster

## Build and run

The intended live-mounted deployment path is [docker/Dockerfile.dev](/Volumes/fastdata/server/salsasite-dev/docker/Dockerfile.dev) plus the commands in [docker/dev-terminal-commands.md](/Volumes/fastdata/server/salsasite-dev/docker/dev-terminal-commands.md).

For local containerized use:

- Build from the repo root with `docker build -f docker/Dockerfile.dev -t salsasite-dev:live .`
- Run with the live-mounted command in [docker/dev-terminal-commands.md](/Volumes/fastdata/server/salsasite-dev/docker/dev-terminal-commands.md)

For Unraid, use [docker/compose.yml](/Volumes/fastdata/server/salsasite-dev/docker/compose.yml) or the commands in [docker/dev-terminal-commands.md](/Volumes/fastdata/server/salsasite-dev/docker/dev-terminal-commands.md). The recommended setup is a single bind mount from the repo root to `/server/live`, with `DATA_DIR`, `MEDIA_ROOT`, `SOURCE_ROOT`, and `POSTER_ROOT` all pointing inside that tree.
