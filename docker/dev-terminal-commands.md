# Live Container Commands

These commands run the app from live files on the Unraid share. The image only provides Node, ffmpeg, and the startup scripts.

Run from the Unraid terminal.

## Remote development note

Codex/development access may be from a remote machine with this repo mounted, while the running Docker container lives on the Unraid server. Do not assume commands run from the remote development shell are executing on the Docker host.

Run Docker lifecycle commands such as `docker build`, `docker run`, `docker logs`, and `docker exec` on the Unraid server unless you have explicitly connected to that host. Browser checks should target the exposed Unraid URL, currently:

```text
http://192.168.0.127:18096
```

## Build the live image

```sh
cd /mnt/user/fastdata/server/salsasite-dev
docker build -f docker/Dockerfile.dev -t salsasite-dev:live .
```

## Stop and remove the current container

```sh
docker stop salsasite-dev || true
docker rm salsasite-dev || true
```

```sh
docker run -d \
  --name salsasite-dev \
  --restart unless-stopped \
  -p 18096:5173 \
  -e HOST=0.0.0.0 \
  -e PORT=5173 \
  -e DATA_DIR=/server/live/migration-data \
  -e MEDIA_ROOT=/server/live/video-moves \
  -e SOURCE_ROOT=/server/live/video-sources \
  -e POSTER_ROOT=/server/live/video-posters \
  -e LIVE_ROOT=/server/live \
  -e CHOKIDAR_USEPOLLING=true \
  -v /mnt/user/fastdata/server/salsasite-dev:/server/live \
  salsasite-dev:live
```

Open:

```text
http://192.168.0.127:18096
```

## Check logs

```sh
docker logs -f salsasite-dev
```

## Video library documentation

The upload, clip editor, render queue, move-linking, and poster behavior is documented in:

```text
docs/video-library.md
```

## Bulk-generate posters

The app generates posters automatically for uploaded sources and rendered clips when `ffmpeg` is available in the container.

To backfill posters for existing move videos:

```sh
docker exec salsasite-dev bash /server/live/scripts/generate_video_posters.sh \
  /server/live/video-moves \
  /server/live/video-posters/video-moves \
  1.0
```

To backfill posters for uploaded source videos:

```sh
docker exec salsasite-dev bash /server/live/scripts/generate_video_posters.sh \
  /server/live/video-sources \
  /server/live/video-posters/video-sources \
  1.0
```

## Optional baked app image

This still reads data, videos, and posters from `/server/live`. It only bakes the compiled frontend app itself.

```sh
cd /mnt/user/fastdata/server/salsasite-dev
docker build -f docker/Dockerfile -t salsasite-dev:latest .

docker stop salsasite-dev || true
docker rm salsasite-dev || true

docker run -d \
  --name salsasite-dev \
  --restart unless-stopped \
  -p 18096:3000 \
  -e PORT=3000 \
  -e HOST=0.0.0.0 \
  -e ORIGIN=http://192.168.0.127:18096 \
  -e DATA_DIR=/server/live/migration-data \
  -e MEDIA_ROOT=/server/live/video-moves \
  -e SOURCE_ROOT=/server/live/video-sources \
  -e POSTER_ROOT=/server/live/video-posters \
  -e NODE_ENV=production \
  -v /mnt/user/fastdata/server/salsasite-dev:/server/live \
  salsasite-dev:latest
```
