#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "Usage: $0 <videomoves_dir> <video_posters_dir> [timestamp_seconds]" >&2
  exit 1
fi

if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "ffmpeg is required on PATH" >&2
  exit 1
fi

videos_dir=$1
posters_dir=$2
timestamp=${3:-1.0}

if [[ ! -d "$videos_dir" ]]; then
  echo "Video directory not found: $videos_dir" >&2
  exit 1
fi

mkdir -p "$posters_dir"

generated_count=0
skipped_count=0
failed_count=0

find "$videos_dir" -type f \( \
  -iname '*.mp4' -o \
  -iname '*.mov' -o \
  -iname '*.m4v' -o \
  -iname '*.webm' \
\) -print0 |
while IFS= read -r -d '' video_file; do
  relative_path=${video_file#"$videos_dir"/}
  relative_without_ext=${relative_path%.*}
  output_file="$posters_dir/$relative_without_ext.jpg"

  mkdir -p "$(dirname "$output_file")"

  if [[ -f "$output_file" ]]; then
    echo "Skipping existing $output_file"
    skipped_count=$((skipped_count + 1))
    continue
  fi

  if ffmpeg \
    -nostdin \
    -hide_banner \
    -loglevel error \
    -y \
    -ss "$timestamp" \
    -i "$video_file" \
    -frames:v 1 \
    -q:v 2 \
    "$output_file"; then
    echo "Generated $output_file"
    generated_count=$((generated_count + 1))
  else
    echo "Failed $video_file" >&2
    rm -f "$output_file"
    failed_count=$((failed_count + 1))
  fi
done

echo "Done: generated=$generated_count skipped=$skipped_count failed=$failed_count"
