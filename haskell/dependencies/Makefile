mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(dir $(mkfile_path))

default: ogg vorbis opus flac sdl rubberband fltk openal ffmpeg lame libsndfile libsamplerate

clean:
	rm -rf root/*

ogg:
	cd libs/ogg && git clean -fdx .
	cd libs/ogg && ./autogen.sh
	cd libs/ogg && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/ogg && make
	cd libs/ogg && make install

vorbis:
	cd libs/vorbis && git clean -fdx .
	cd libs/vorbis && ./autogen.sh
	cd libs/vorbis && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/vorbis && make
	cd libs/vorbis && make install

opus:
	cd libs/opus && git clean -fdx .
	cd libs/opus && ./autogen.sh
	cd libs/opus && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/opus && make
	cd libs/opus && make install

flac:
	cd libs/flac && git clean -fdx .
	cd libs/flac && ./autogen.sh
	cd libs/flac && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/flac && make
	cd libs/flac && make install

sdl:
	cd libs/SDL && git clean -fdx .
	cd libs/SDL && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/SDL && make
	cd libs/SDL && make install

rubberband:
	cd libs/rubberband && git clean -fdx .
	cd libs/rubberband && make -f ../../extra/Makefile.rubberband
	mkdir -p $(current_dir)/root/lib
	cp libs/rubberband/lib/* $(current_dir)/root/lib
	mkdir -p $(current_dir)/root/include
	cp -R libs/rubberband/rubberband $(current_dir)/root/include
	./make-pkg-config extra/rubberband.pc.in $(current_dir)/root

fltk:
	cd libs/fltk && git clean -fdx .
	cd libs/fltk && autoconf
	cd libs/fltk && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/fltk && make
	cd libs/fltk && make install

openal:
	cd libs/openal-soft && git clean -fdx .
	cd libs/openal-soft/build && cmake -D CMAKE_INSTALL_PREFIX=$(current_dir)/root -D ALSOFT_EXAMPLES=OFF ..
	cd libs/openal-soft/build && make
	cd libs/openal-soft/build && make install

ffmpeg:
	cd libs/FFmpeg && git clean -fdx .
	cd libs/FFmpeg && ./configure --enable-shared --disable-encoders --prefix=$(current_dir)/root
	cd libs/FFmpeg && make
	cd libs/FFmpeg && make install

lame:
	cd libs/LAME && git clean -fdx .
	cd libs/LAME && ./configure --prefix=$(current_dir)/root --disable-decoder --disable-frontend
	cd libs/LAME && make
	cd libs/LAME && make install

libsndfile:
	cd libs/libsndfile && git clean -fdx .
	cd libs/libsndfile && ./autogen.sh
	cd libs/libsndfile && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/libsndfile && make
	cd libs/libsndfile && make install

libsamplerate:
	cd libs/libsamplerate && git clean -fdx .
	cd libs/libsamplerate && ./autogen.sh
	cd libs/libsamplerate && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig PKG_CONFIG="pkg-config --env-only" ./configure --prefix=$(current_dir)/root
	cd libs/libsamplerate && make
	cd libs/libsamplerate && make install

