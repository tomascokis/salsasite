# Unraid Deployment

## Container shape

The new site is designed to run as a single Node container, not as an R runtime plus nginx.
The preferred Unraid setup is a live-mounted container: the image provides the runtime and dependencies, while the repo files stay on the mounted share.

Near-term architecture improvements must preserve this shape: one Docker container, one mounted project tree, and live-editable application/data files. Improvements may add stronger persistence, authentication, cache invalidation, or job recovery inside that container, but must not require splitting the app into multiple services unless the deployment contract is explicitly changed.

Main files:

- [docker/Dockerfile](/Volumes/fastdata/server/salsasite-dev/docker/Dockerfile)
- [docker/start.sh](/Volumes/fastdata/server/salsasite-dev/docker/start.sh)
- [docker/compose.yml](/Volumes/fastdata/server/salsasite-dev/docker/compose.yml)

## Default runtime

- Port: `3000`
- Recommended live mount root in container: `/server/live`
- Data directory in container: `/server/live/data/live`
- Move video directory in container: `/server/live/data/live/media/video-moves`
- Source video directory in container: `/server/live/data/live/media/video-sources`
- Poster directory in container: `/server/live/data/live/media/video-posters`
- SQLite app state: `/server/live/data/live/app-state.sqlite`

For the full video catalog, upload, clip-render, and poster workflow, see [docs/video-library.md](/Volumes/fastdata/server/salsasite-dev/docs/video-library.md).

## Account setup

Authentication is local to the app-state SQLite database. There is no public signup flow. Create accounts from inside the running container after dependencies are installed:

```bash
docker exec -it salsasite-dev npm run user:create -- --username admin --role admin
docker exec -it salsasite-dev npm run user:create -- --username viewer --role viewer
```

To change an existing user's password or role:

```bash
docker exec -it salsasite-dev npm run user:password -- --username admin --role admin
```

For non-interactive automation, pipe the password and pass `--password-stdin`.

## Unraid setup

Use [docker/Dockerfile.dev](/Volumes/fastdata/server/salsasite-dev/docker/Dockerfile.dev) for the live-mounted setup, or [docker/Dockerfile](/Volumes/fastdata/server/salsasite-dev/docker/Dockerfile) if you specifically want a compiled frontend app image.

Development may happen from a remote machine with the Unraid share mounted. That machine is not necessarily the Docker host. Run container lifecycle commands on the Unraid server itself, and verify the app through the exposed server URL rather than assuming a local dev server on the remote machine represents the running container.

Recommended mounts:

- Host repo root `/mnt/user/fastdata/server/salsasite-dev` -> container `/server/live`

Recommended environment:

- `PORT=3000`
- `HOST=0.0.0.0`
- `ORIGIN=http://your-unraid-host:3000`
- `DATA_DIR=/server/live/data/live`
- `MEDIA_ROOT=/server/live/data/live/media/video-moves`
- `SOURCE_ROOT=/server/live/data/live/media/video-sources`
- `POSTER_ROOT=/server/live/data/live/media/video-posters`
- Optional: `POSTER_TIMESTAMP_SECONDS=1.0`

## Data refresh workflow

The container does not need R or Quarto at runtime, and the legacy R/Quarto source files have been retired.

When workbook-derived reference data changes, regenerate the app-native workbook export:

```bash
/Users/tomascokis/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 scripts/export_data_reference.py
```

The checked-in `data/live/bootstrap/**/*.json` files are first-run seed artifacts for the current SvelteKit/SQLite app. SQLite under `data/live/app-state.sqlite` is the live app data store after bootstrap.

With the live-mounted setup, changes under `/mnt/user/fastdata/server/salsasite-dev` are read directly from the share. Rebuild only when the container dependencies or startup behavior change.

## Current limitations

- I could not run `npm install` or `vite build` locally in this workspace because no package manager is available on `PATH` here.
- The app scaffold and Docker build are set up for the target container environment, but the final dependency install and compile need to happen during Docker build or in a Node environment with `npm`.
