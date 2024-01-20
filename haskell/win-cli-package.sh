#!/bin/bash

VERSION=$(cat -)
if [[ ! $VERSION =~ ^[0-9]{8}$ ]]; then
  echo "Not a valid program version."
  exit 1
fi
ZIPNAME=onyx-command-line-$VERSION-windows-x64
rm -rf $ZIPNAME $ZIPNAME.zip
mv win $ZIPNAME
stack exec -- zip -r $ZIPNAME.zip $ZIPNAME
