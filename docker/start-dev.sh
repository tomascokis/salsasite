#!/bin/sh
set -eu

LIVE_ROOT=${LIVE_ROOT:-/server/live}
FRONTEND_DIR=${FRONTEND_DIR:-$LIVE_ROOT/frontend}

if [ ! -f "$FRONTEND_DIR/package.json" ]; then
  FRONTEND_DIR=/app/frontend
fi

cd "$FRONTEND_DIR"

if [ ! -f package.json ]; then
  echo "Missing frontend package.json. Mount the repo root into $LIVE_ROOT or provide /app/frontend." >&2
  exit 1
fi

if [ ! -x node_modules/.bin/vite ] \
  || ! node -e "require.resolve('@rollup/rollup-linux-x64-gnu'); require.resolve('@rolldown/binding-linux-x64-gnu')" >/dev/null 2>&1; then
  npm install
fi

exec npm run dev -- --host "${HOST:-0.0.0.0}" --port "${PORT:-5173}"
