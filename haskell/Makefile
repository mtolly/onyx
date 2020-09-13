.PHONY: usage install-resources mac mac-deps win win-deps docker linux ubuntu-deps arch-deps ghci
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
	@echo "  make linux (only runs on older linux)"
	@echo "  make docker (runs on any linux)"
	@echo "    packages app for distribution"

install-resources:
	./make-resources.sh $(HOME)/.local/bin/onyx-resources
	cp -R rb3-updates $(HOME)/.local/bin/onyx-resources/
	stack exec -- onyx-package version-write $(HOME)/.local/bin/onyx-resources/README.txt

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
	stack exec -- onyx-package changes
	stack exec -- onyx-package version-write mac/Onyx.app/Contents/Info.plist mac/README.txt mac/Onyx.app/Contents/MacOS/onyx-resources/README.txt
	stack exec -- onyx-package version-print | ./mac-package.sh

mac-deps:
	brew install libsndfile libsamplerate lame rubberband dylibbundler wine mpg123 autoconf openal-soft

win:
	stack build
	rm -rf win/
	mkdir win/
	stack exec which onyx | xargs -I{} cp {} win/
	cp /mingw32/bin/*.dll win/
	stack exec strip win/onyx.exe
	cp LICENSE.txt win/LICENSE.txt
	cp README.md win/README.txt
	cp CHANGES.md win/CHANGES.txt
	./make-resources.sh win/onyx-resources
	stack exec -- onyx-package changes
	stack exec -- onyx-package version-write win/README.txt win/onyx-resources/README.txt
	stack exec -- onyx-package dlls win/onyx.exe
	stack exec -- onyx-package nsis
	"/c/Program Files (x86)/NSIS/Bin/makensis.exe" installer.nsi

win-deps:
	pacman -Sy mingw-w64-i686-pkg-config mingw-w64-i686-openal mingw-w64-i686-libsndfile mingw-w64-i686-libsamplerate mingw-w64-i686-lame mingw-w64-i686-rubberband mingw-w64-i686-mpg123 wget tar unzip zip man autoconf automake

icon/icon.o:
	stack exec -- windres -i icon/icon.rc icon/icon.o

# runs stack in docker mode and then linuxdeployqt inside that container
docker:
	docker build -t onyxite/onyx .
	stack --docker --docker-image=onyxite/onyx build
	rm -rf Onyx.AppDir/usr/bin/*
	rm -rf Onyx.AppDir/usr/lib/*
	rm -rf Onyx.AppDir/usr/share/metainfo/*
	stack --docker --docker-image=onyxite/onyx exec which onyx | xargs -I{} cp {} Onyx.AppDir/usr/bin/onyx
	strip Onyx.AppDir/usr/bin/onyx
	./make-resources.sh Onyx.AppDir/usr/bin/onyx-resources
	cp icon/icon.png Onyx.AppDir/usr/share/icons/hicolor/256x256/apps/onyx.png
	cp org.onyxite.onyx.appdata.xml Onyx.AppDir/usr/share/metainfo/
	stack --docker --docker-image=onyxite/onyx exec -- onyx-package changes
	stack --docker --docker-image=onyxite/onyx exec -- onyx-package version-write Onyx.AppDir/usr/share/metainfo/org.onyxite.onyx.appdata.xml Onyx.AppDir/usr/bin/onyx-resources/README.txt
	stack --docker --docker-image=onyxite/onyx exec -- onyx-package version-print \
		| xargs -I{} docker run -v $(shell pwd):/mnt onyxite/onyx bash -c \
			'cd /mnt && env VERSION={} linuxdeployqt Onyx.AppDir/usr/share/applications/onyx.desktop -appimage'

# this runs direct stack and linuxdeployqt
# (the linuxdeployqt will fail on recent linux systems)
linux:
	stack build
	rm -rf Onyx.AppDir/usr/bin/*
	rm -rf Onyx.AppDir/usr/lib/*
	rm -rf Onyx.AppDir/usr/share/metainfo/*
	stack exec which onyx | xargs -I{} cp {} Onyx.AppDir/usr/bin/onyx
	strip Onyx.AppDir/usr/bin/onyx
	./make-resources.sh Onyx.AppDir/usr/bin/onyx-resources
	cp icon/icon.png Onyx.AppDir/usr/share/icons/hicolor/256x256/apps/onyx.png
	cp org.onyxite.onyx.appdata.xml Onyx.AppDir/usr/share/metainfo/
	stack exec -- onyx-package changes
	stack exec -- onyx-package version-write Onyx.AppDir/usr/share/metainfo/org.onyxite.onyx.appdata.xml Onyx.AppDir/usr/bin/onyx-resources/README.txt
	stack exec -- onyx-package version-print \
		| xargs -I{} env VERSION={} linuxdeployqt Onyx.AppDir/usr/share/applications/onyx.desktop -appimage

ubuntu-deps:
	apt-get -y install libsndfile-dev libsamplerate-dev libmp3lame-dev librubberband-dev libmpg123-dev freeglut3-dev autoconf libxft-dev libopenal-dev unzip

arch-deps:
	pacman -S libsndfile libsamplerate lame rubberband mpg123 openal

ghci:
	stack ghci onyxite-customs-lib
