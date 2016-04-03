.PHONY: mac mac-deps win win-deps usage

usage:
	@echo "Targets:"
	@echo "  make mac-deps"
	@echo "  make mac"
	@echo "  make win-deps"
	@echo "  make win"

mac:
	rm -rf mac/
	mkdir mac/
	stack build .
	cp "$(shell stack exec which onyx)" mac/
	dylibbundler -x mac/onyx -b -d mac/ -p '@executable_path' -of
	strip mac/onyx

mac-deps:
	stack setup
	brew install libsndfile libsamplerate lame

win:
	rm -rf win/
	mkdir win/
	stack build .
	cp "$(shell stack exec which onyx)" win/
	cp /mingw32/bin/*.dll win/
	stack exec strip win/onyx.exe

win-deps:
	stack setup
	pacman -Sy
	pacman -S mingw-w64-i686-libsndfile mingw-w64-i686-libsamplerate mingw-w64-i686-lame
