.PHONY: mac win usage

usage:
	echo "Type:"
	echo "make mac, make win"

mac:
	rm -rf mac/
	mkdir mac/
	stack build
	cp "$(shell stack exec which onyxbuild)" mac/
	dylibbundler -x mac/onyxbuild -b -d mac/ -p '@executable_path' -of

mac-deps:
	stack setup
	brew install libsndfile libsamplerate

win:
	rm -rf win/
	mkdir win/
	stack build --extra-lib-dirs=/mingw32/lib --extra-include-dirs=/mingw32/include
	cp "$(shell stack exec which onyxbuild)" win/
	cp /mingw32/bin/*.dll win/

win-deps:
	stack setup
	pacman -S mingw-w64-i686-libsndfile mingw-w64-i686-libsamplerate
