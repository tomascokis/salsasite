#!/bin/sh
set -eu

cd /app/frontend
exec node build
