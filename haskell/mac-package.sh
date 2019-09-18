#!/bin/bash

VERSION=$(cat -)
ZIPNAME=onyx-$VERSION-macos-x64
rm -rf $ZIPNAME $ZIPNAME.zip
mv mac $ZIPNAME
zip -r $ZIPNAME.zip $ZIPNAME
