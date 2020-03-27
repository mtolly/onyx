.PHONY: usage install-resources mac mac-deps win win-deps linux ubuntu-deps arch-deps ghci
SHELL=/bin/bash

usage:
	@echo "Targets:"
	@echo "  make install-resources"
	@echo "    puts required files in ~/.local/bin"
	@echo "  make mac-deps"
	@echo "  make win-deps"
	@echo "  make ubuntu-deps"
	@echo "  make arch-deps"
	@echo "    installs C libs via homebrew/pacman/apt"
	@echo "  make mac"
	@echo "  make win"
	@echo "  make linux"
	@echo "    packages app for distribution"

install-resources:
	./make-resources.sh $(HOME)/.local/bin/onyx-resources
	cp -R rb3-updates $(HOME)/.local/bin/onyx-resources/

mac:
	rm -rf Onyx.app/Contents/MacOS/*
	stack build
	stack exec which onyx | xargs -I{} cp {} Onyx.app/Contents/MacOS/onyx
	strip Onyx.app/Contents/MacOS/onyx
	rm -rf Onyx.app/Contents/libs/*.dylib
	dylibbundler -cd -of -b -x Onyx.app/Contents/MacOS/onyx -d Onyx.app/Contents/libs/
	./make-resources.sh Onyx.app/Contents/MacOS/onyx-resources
	rm -rf mac/
	mkdir mac/
	cp -R Onyx.app LICENSE.txt mac/
	cp README.md mac/README.txt
	cp CHANGES.md mac/CHANGES.txt
	stack exec -- onyx-package mac/Onyx.app/Contents/Info.plist mac/README.txt | ./mac-package.sh

mac-deps:
	brew install libsndfile libsamplerate lame rubberband dylibbundler wine mpg123 autoconf openal-soft

win:
	stack build
	rm -rf win/
	mkdir win/
	stack exec which onyx | xargs -I{} cp {} win/
	cp /mingw64/bin/*.dll win/
	stack exec strip win/onyx.exe
	cp LICENSE.txt win/LICENSE.txt
	cp README.md win/README.txt
	cp CHANGES.md win/CHANGES.txt
	./make-resources.sh win/onyx-resources
	stack exec -- onyx-package win/README.txt
	"/c/Program Files (x86)/NSIS/Bin/makensis.exe" installer.nsi

win-deps:
	pacman -Sy mingw-w64-x86_64-pkg-config mingw-w64-x86_64-libsndfile mingw-w64-x86_64-libsamplerate mingw-w64-x86_64-lame mingw-w64-x86_64-rubberband mingw-w64-x86_64-mpg123 wget tar unzip zip man autoconf automake

linux:
	rm -rf Onyx.AppDir/usr/bin/*
	rm -rf Onyx.AppDir/usr/lib/*
	rm -rf Onyx.AppDir/usr/share/metainfo/*
	stack build
	stack exec which onyx | xargs -I{} cp {} Onyx.AppDir/usr/bin/onyx
	strip Onyx.AppDir/usr/bin/onyx
	./make-resources.sh Onyx.AppDir/usr/bin/onyx-resources
	cp icon/icon.png Onyx.AppDir/usr/share/icons/hicolor/256x256/apps/onyx.png
	cp org.onyxite.onyx.appdata.xml Onyx.AppDir/usr/share/metainfo/
	stack exec -- onyx-package Onyx.AppDir/usr/share/metainfo/org.onyxite.onyx.appdata.xml | xargs -I{} env VERSION={} linuxdeployqt Onyx.AppDir/usr/share/applications/onyx.desktop -appimage
	# centos 7 also needs these to run the appimage: yum install mesa-libGL fontconfig

icon/icon.o:
	stack exec -- windres -i icon/icon.rc icon/icon.o

ubuntu-deps:
	apt install libsndfile-dev libsamplerate-dev libmp3lame-dev librubberband-dev libmpg123-dev freeglut3-dev autoconf libxft-dev libopenal-dev unzip

arch-deps:
	pacman -S libsndfile libsamplerate lame rubberband mpg123

ghci:
	stack ghci onyxite-customs-lib
