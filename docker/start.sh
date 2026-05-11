#!/bin/bash

mkdir -p /site

echo "Building salsa site..."
Rscript /project/build.R || echo "Initial build failed"

echo "Starting nginx..."
nginx

while true; do
  echo "Rebuilding salsa site..."
  Rscript /project/build.R || echo "Build failed"
  sleep 3600
done