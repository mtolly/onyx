#!/bin/bash
set -e
set -u

cd "$1"
onyx player --plan "$2"
cd -
cp "$1/gen/plan/$2/web/song.js" build/
cp "$1/gen/plan/$2/web/audio-mp3.js" build/
cp "$1/gen/plan/$2/web/audio-ogg.js" build/
