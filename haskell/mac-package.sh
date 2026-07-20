#!/bin/bash

VERSION=$(cat -)
if [[ ! $VERSION =~ ^[0-9]{8}$ ]]; then
  echo "Not a valid program version."
  exit 1
fi
case $(uname -m) in
  arm64 )  ARCH=arm64 ;;
  x86_64 ) ARCH=x64 ;;
  * )
    echo "Unknown architecture: $(uname -m)"
    exit 1
esac
ZIPNAME=onyx-$VERSION-macos-$ARCH
rm -rf $ZIPNAME $ZIPNAME.zip
mv mac $ZIPNAME
zip -r $ZIPNAME.zip $ZIPNAME
