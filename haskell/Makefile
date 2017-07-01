.PHONY: usage install-magma mac mac-deps win win-deps ubuntu-deps
SHELL=/bin/bash

ONYXVERSION=20170701

usage:
	@echo "Targets:"
	@echo "  make install-magma"
	@echo "    puts Magma files in ~/.local/bin"
	@echo "  make mac-deps"
	@echo "  make win-deps"
	@echo "  make ubuntu-deps"
	@echo "    installs C libs via homebrew/pacman/apt"
	@echo "  make mac"
	@echo "  make win"
	@echo "    packages app for distribution"

install-magma:
	rm -rf $(HOME)/.local/bin/magma-{v1,v2,common,ogg2mogg}
	rm -rf $(HOME)/.local/bin/x360-rb3pkg
	cp -R vendors/magma-{v1,v2,common,ogg2mogg} $(HOME)/.local/bin/
	cp -R vendors/x360-rb3pkg $(HOME)/.local/bin/

mac:
	# stack build .
	cp $(shell stack exec which onyx) Onyx.app/Contents/MacOS/onyx
	strip Onyx.app/Contents/MacOS/onyx
	rm -rf Onyx.app/Contents/libs/*.dylib
	dylibbundler -cd -of -b -x Onyx.app/Contents/MacOS/onyx -d Onyx.app/Contents/libs/
	rm -rf Onyx.app/Contents/MacOS/magma-{v1,v2,common,ogg2mogg}
	rm -rf Onyx.app/Contents/MacOS/x360-rb3pkg
	cp -R vendors/magma-{v1,v2,common,ogg2mogg} Onyx.app/Contents/MacOS/
	cp -R vendors/x360-rb3pkg Onyx.app/Contents/MacOS/
	rm -rf mac/
	mkdir mac/
	cp -R Onyx.app LICENSE.txt mac/
	cp README.md mac/README.txt
	sed -i '' 's/_ONYXVERSION_/$(ONYXVERSION)/' mac/Onyx.app/Contents/Info.plist
	sed -i '' 's/_ONYXVERSION_/$(ONYXVERSION)/' mac/README.txt

mac-deps:
	brew install libsndfile libsamplerate lame sdl2 sdl2_ttf rubberband botan

win:
	# stack build .
	rm -rf win/
	mkdir win/
	cp "$(shell stack exec which onyx)" win/
	cp /mingw32/bin/*.dll win/
	stack exec strip win/onyx.exe
	cp LICENSE.txt win/LICENSE.txt
	cp README.md win/README.txt
	cp -R vendors/magma-{v1,v2,common,ogg2mogg} win/
	cp -R vendors/x360-rb3pkg win/
	sed -b -i 's/_ONYXVERSION_/$(ONYXVERSION)/' win/README.txt

win-deps:
	pacman -Sy mingw-w64-i686-pkg-config mingw-w64-i686-libsndfile mingw-w64-i686-libsamplerate mingw-w64-i686-lame mingw-w64-i686-SDL2 mingw-w64-i686-SDL2_ttf mingw-w64-i686-rubberband
	# TODO mingw-w64-i686-libbotan doesn't make .dll, see BUILD.md

icon/icon.o:
	stack exec -- windres -i icon/icon.rc icon/icon.o

ubuntu-deps:
	apt install libsndfile-dev libsamplerate-dev libmp3lame-dev libsdl2-dev libsdl2-ttf-dev librubberband-dev
	# install libbotan from source, ubuntu package is v1 but we need v2
