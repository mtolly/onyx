#!/bin/bash
set -e
set -u

rm -rf "$1"
cp -R resources "$1"
cp -R ../player/www "$1"/player
cp ../player/build/app.min.js "$1"/player/
