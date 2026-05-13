mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(dir $(mkfile_path))

# need to force ffmpeg to use Clang on Windows
ifeq ($(OS),Windows_NT)
ffmpeg_cc := --cc=clang --cxx=clang++ --objcc=clang --dep-cc=clang
else
ffmpeg_cc :=
endif

# libraries we are currently building locally
default: ogg vorbis ffmpeg lame libsamplerate fltk

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
	# TODO previously we hacked fltkhs to disable kdialog support,
	# do we still need to do that or is it fixed up now?
	# pen support is off because it fails to build on my windows 10 22h2 vm
	cd libs/fltk && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig cmake . -B build -DCMAKE_INSTALL_PREFIX=$(current_dir)/root \
		-DFLTK_USE_SYSTEM_LIBJPEG=OFF -DFLTK_USE_SYSTEM_ZLIB=OFF -DFLTK_USE_SYSTEM_LIBPNG=OFF \
		-DFLTK_OPTION_PEN_SUPPORT=OFF
	cd libs/fltk && cmake --build build
	cd libs/fltk && cmake --install build

openal:
	# does not build on ubuntu 16.04
	cd libs/openal-soft/build && cmake -D CMAKE_INSTALL_PREFIX=$(current_dir)/root -D ALSOFT_EXAMPLES=OFF ..
	cd libs/openal-soft/build && make
	cd libs/openal-soft/build && make install

ffmpeg:
	# disabling AVX-512, AVX-512ICL, XOP, FMA4, and optionally AVX2, FMA3,
	# were recommended by Claude Code for CPU compatibility.
	# for encoders/muxers we only need ogg vorbis, ogg opus, and wav
	cd libs/FFmpeg && ./configure --enable-shared --disable-encoders --disable-muxers \
		--enable-encoder=vorbis --enable-encoder=opus --enable-encoder=pcm_s16le \
		--enable-muxer=ogg --enable-muxer=wav \
		--disable-avx512 --disable-avx512icl --disable-xop --disable-fma4 --disable-avx2 --disable-fma3 \
		--prefix=$(current_dir)/root $(ffmpeg_cc)
	cd libs/FFmpeg && make
	cd libs/FFmpeg && make install

lame:
	cd libs/LAME && ./configure --prefix=$(current_dir)/root --disable-decoder --disable-frontend
	cd libs/LAME && make
	cd libs/LAME && make install

libsamplerate:
	cd libs/libsamplerate && ./autogen.sh
	# without --disable-sndfile, example program has issues building on windows
	cd libs/libsamplerate && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig ./configure --prefix=$(current_dir)/root --disable-sndfile
	cd libs/libsamplerate && make
	cd libs/libsamplerate && make install
