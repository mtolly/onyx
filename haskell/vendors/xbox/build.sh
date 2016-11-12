#!/bin/bash
set -e
set -u
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

cd "$DIR/X360"
xbuild /p:Configuration=Release
cd ../rb3pkg
cp ../X360/bin/Release/X360.dll .
xbuild /p:Configuration=Release
