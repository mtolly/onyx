.PHONY: usage install-resources mac mac-deps win win-deps ubuntu-deps
SHELL=/bin/bash

usage:
	@echo "Targets:"
	@echo "  make install-resources"
	@echo "    puts required files in ~/.local/bin"
	@echo "  make mac-deps"
	@echo "  make win-deps"
	@echo "  make ubuntu-deps"
	@echo "    installs C libs via homebrew/pacman/apt"
	@echo "  make mac"
	@echo "  make win"
	@echo "    packages app for distribution"

install-resources:
	rm -rf $(HOME)/.local/bin/magma-{v1,v2,common,ogg2mogg}
	rm -rf $(HOME)/.local/bin/rb3-updates
	cp -R vendors/magma-{v1,v2,common,ogg2mogg} $(HOME)/.local/bin/
	cp -R vendors/rb3-updates $(HOME)/.local/bin/
	cp vendors/itaijidict $(HOME)/.local/bin/
	cp vendors/kanwadict $(HOME)/.local/bin/

mac:
	stack build
	stack exec which onyx | xargs -I{} cp {} Onyx.app/Contents/MacOS/onyx
	strip Onyx.app/Contents/MacOS/onyx
	rm -rf Onyx.app/Contents/libs/*.dylib
	dylibbundler -cd -of -b -x Onyx.app/Contents/MacOS/onyx -d Onyx.app/Contents/libs/
	rm -rf Onyx.app/Contents/MacOS/magma-{v1,v2,common,ogg2mogg}
	rm -rf Onyx.app/Contents/MacOS/rb3-updates
	cp -R vendors/magma-{v1,v2,common,ogg2mogg} Onyx.app/Contents/MacOS/
	# cp -R vendors/rb3-updates Onyx.app/Contents/MacOS/
	cp vendors/itaijidict Onyx.app/Contents/MacOS/
	cp vendors/kanwadict Onyx.app/Contents/MacOS/
	rm -rf mac/
	mkdir mac/
	cp -R Onyx.app LICENSE.txt mac/
	cp README.md mac/README.txt
	cp CHANGES.md mac/CHANGES.txt
	stack exec -- onyx-package mac/Onyx.app/Contents/Info.plist mac/README.txt | ./mac-package.sh

mac-deps:
	brew install libsndfile libsamplerate lame rubberband dylibbundler wine mpg123

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
	cp -R vendors/magma-{v1,v2,common,ogg2mogg} win/
	# cp -R vendors/rb3-updates win/
	cp vendors/itaijidict win/
	cp vendors/kanwadict win/
	stack exec -- onyx-package win/README.txt
	"/c/Program Files (x86)/NSIS/Bin/makensis.exe" installer.nsi

win-deps:
	pacman -Sy mingw-w64-x86_64-pkg-config mingw-w64-x86_64-libsndfile mingw-w64-x86_64-libsamplerate mingw-w64-x86_64-lame mingw-w64-x86_64-rubberband mingw-w64-x86_64-mpg123 wget tar unzip zip man autoconf automake

icon/icon.o:
	stack exec -- windres -i icon/icon.rc icon/icon.o

ubuntu-deps:
	apt install libsndfile-dev libsamplerate-dev libmp3lame-dev librubberband-dev libmpg123-dev

arch-deps:
	pacman -S libsndfile libsamplerate lame rubberband mpg123

ghci:
	stack ghci onyxite-customs-lib
