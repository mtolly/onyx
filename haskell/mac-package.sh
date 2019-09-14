#!/bin/bash

VERSION=$(cat -)
ZIPNAME=onyx-$VERSION-macos-x64
mv mac $ZIPNAME
zip -r $ZIPNAME.zip $ZIPNAME
