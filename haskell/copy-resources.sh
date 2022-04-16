#!/bin/bash
set -e
set -u

cp icon/icon.png resources/icon.png
cp -R ../player/www resources/player
cp ../player/build/app.min.js resources/player/
cp README.md resources/README.txt
cp CHANGES.md resources/CHANGES.txt
cp LICENSE.txt resources/LICENSE.txt
cp CREDITS.txt resources/CREDITS.txt
mkdir -p resources/makefsb4
cp makefsb4/*.{exe,dll} resources/makefsb4
