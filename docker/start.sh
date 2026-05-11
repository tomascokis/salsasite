#!/bin/bash

cd /salsasite

echo "Building salsa site..."
Rscript /salsasite/build.R || echo "Initial build failed"

echo "Starting nginx..."
nginx

while true; do
  echo "Rebuilding salsa site..."
  cd /salsasite
  Rscript /salsasite/build.R || echo "Build failed"
  sleep 3600
done