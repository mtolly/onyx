.PHONY: mac mac-deps win win-deps usage

usage:
	@echo "Targets:"
	@echo "  make mac-deps"
	@echo "  make mac"
	@echo "  make win-deps"
	@echo "  make win"

mac:
	# stack build .
	cp $(shell stack exec which onyx) Onyx.app/Contents/MacOS/onyx
	strip Onyx.app/Contents/MacOS/onyx
	rm -rf Onyx.app/Contents/libs/*.dylib
	dylibbundler -cd -of -b -x Onyx.app/Contents/MacOS/onyx -d Onyx.app/Contents/libs/

mac-deps:
	stack setup
	brew install libsndfile libsamplerate lame sdl2 sdl2_ttf

win:
	rm -rf win/
	mkdir win/
	# stack build .
	cp "$(shell stack exec which onyx)" win/
	cp /mingw32/bin/*.dll win/
	stack exec strip win/onyx.exe
	cp batch/*.bat batch/*.hta batch/*.eot batch/*.txt win/

win-deps:
	stack setup
	pacman -Sy mingw-w64-i686-libsndfile mingw-w64-i686-libsamplerate mingw-w64-i686-lame mingw-w64-i686-SDL2 mingw-w64-i686-SDL2_ttf mingw-w64-i686-pkg-config
