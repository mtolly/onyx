mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(dir $(mkfile_path))

# libraries we are currently building locally
default: ogg vorbis opus flac ffmpeg lame libsndfile libsamplerate

clean:
	rm -rf root/*

ogg:
	cd libs/ogg && ./autogen.sh
	cd libs/ogg && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/ogg && make
	cd libs/ogg && make install

vorbis:
	cd libs/vorbis && ./autogen.sh
	cd libs/vorbis && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/vorbis && make
	cd libs/vorbis && make install

opus:
	# update_version doesn't work in the copied-to-docker git repo.
	# and if we don't set the version, then libsndfile won't use any external libs
	cd libs/opus && echo 'PACKAGE_VERSION="1.3.1"' > package_version
	cd libs/opus && ./autogen.sh
	cd libs/opus && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/opus && make
	cd libs/opus && make install

flac:
	cd libs/flac && ./autogen.sh
	cd libs/flac && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/flac && make
	cd libs/flac && make install

sdl:
	cd libs/SDL && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/SDL && make
	cd libs/SDL && make install

rubberband:
	# TODO this is currently broken
	cd libs/rubberband && make -f ../../extra/Makefile.rubberband
	mkdir -p $(current_dir)/root/lib
	cp libs/rubberband/lib/* $(current_dir)/root/lib
	mkdir -p $(current_dir)/root/include
	cp -R libs/rubberband/rubberband $(current_dir)/root/include
	./make-pkg-config extra/rubberband.pc.in $(current_dir)/root

fltk:
	# TODO this needs to be matched with a working fltkhs version
	cd libs/fltk && autoconf
	cd libs/fltk && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/fltk && make
	cd libs/fltk && make install

openal:
	# does not build on ubuntu 16.04
	cd libs/openal-soft/build && cmake -D CMAKE_INSTALL_PREFIX=$(current_dir)/root -D ALSOFT_EXAMPLES=OFF ..
	cd libs/openal-soft/build && make
	cd libs/openal-soft/build && make install

ffmpeg:
	cd libs/FFmpeg && ./configure --enable-shared --disable-encoders --prefix=$(current_dir)/root
	cd libs/FFmpeg && make
	cd libs/FFmpeg && make install

lame:
	cd libs/LAME && ./configure --prefix=$(current_dir)/root --disable-decoder --disable-frontend
	cd libs/LAME && make
	cd libs/LAME && make install

libsndfile:
	# repo is on 1.0.31 because of https://github.com/libsndfile/libsndfile/issues/827
	cd libs/libsndfile && ./autogen.sh
	cd libs/libsndfile && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root
	cd libs/libsndfile && make
	cd libs/libsndfile && make install

libsamplerate:
	cd libs/libsamplerate && ./autogen.sh
	# without --disable-sndfile, example program has issues building on windows
	cd libs/libsamplerate && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root --disable-sndfile
	cd libs/libsamplerate && make
	cd libs/libsamplerate && make install
