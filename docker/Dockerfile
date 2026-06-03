FROM node:22-bookworm-slim AS build

WORKDIR /app/frontend

COPY frontend/package.json ./
RUN npm install

COPY frontend ./

RUN npm run build

FROM node:22-bookworm-slim AS runtime

WORKDIR /app/frontend

RUN apt-get update \
  && apt-get install -y --no-install-recommends ffmpeg \
  && rm -rf /var/lib/apt/lists/*

ENV NODE_ENV=production
ENV PORT=3000
ENV HOST=0.0.0.0
ENV ORIGIN=http://localhost:3000
ENV LIVE_ROOT=/server/live
ENV DATA_DIR=/server/live/data/live
ENV MEDIA_ROOT=/server/live/data/live/media/video-moves
ENV SOURCE_ROOT=/server/live/data/live/media/video-sources
ENV POSTER_ROOT=/server/live/data/live/media/video-posters

COPY --from=build /app/frontend/build ./build
COPY --from=build /app/frontend/node_modules ./node_modules
COPY docker/start.sh /start.sh

RUN mkdir -p /server/live/data/live/media/video-moves \
  /server/live/data/live/media/video-sources \
  /server/live/data/live/media/video-posters \
  && chmod +x /start.sh

EXPOSE 3000

CMD ["/start.sh"]
