#!/bin/bash
set -e
set -u

cd "$1"
onyx build gen/plan/album/web "${@:2}"
cd -
cp "$1/gen/plan/album/web/song.js" www/
cp "$1/gen/plan/album/web/preview-audio.mp3" www/
cp "$1/gen/plan/album/web/preview-audio.ogg" www/
