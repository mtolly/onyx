mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(dir $(mkfile_path))

# need to force ffmpeg to use Clang on Windows
ifeq ($(OS),Windows_NT)
ffmpeg_cc := --cc=clang --cxx=clang++ --objcc=clang --dep-cc=clang
else
ffmpeg_cc :=
endif

# need pango for FLTK glyph fallback for non-ASCII text on Linux
ifeq ($(shell uname),Linux)
fltk_pango := -DFLTK_USE_PANGO=ON
else
fltk_pango :=
endif

# libraries we are currently building locally
default: ogg vorbis ffmpeg lame libsamplerate rubberband fltk openal

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
	# note, if we don't set libdir then it uses e.g. `lib/x86_64-linux-gnu` and then ghc can't find rubberband.pc
	cd libs/rubberband && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig meson setup build \
		--prefix $(current_dir)/root \
		-Dlibdir=lib \
		-Dauto_features=disabled -Ddefault_library=shared \
		-Dresampler=libsamplerate
	cd libs/rubberband && ninja -C build
	cd libs/rubberband && ninja -C build install

fltk:
	# pen support is off because it fails to build on my windows 10 22h2 vm.
	cd libs/fltk && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig cmake . -B build -DCMAKE_INSTALL_PREFIX=$(current_dir)/root \
		-DFLTK_USE_SYSTEM_LIBJPEG=OFF -DFLTK_USE_SYSTEM_ZLIB=OFF -DFLTK_USE_SYSTEM_LIBPNG=OFF \
		-DFLTK_OPTION_PEN_SUPPORT=OFF $(fltk_pango)
	cd libs/fltk && cmake --build build
	cd libs/fltk && cmake --install build

openal:
	# note, at some point newer openal-soft commits require a newer cmake (3.18) than is in ubuntu 20.04 (3.16).
	# CMAKE_POLICY_VERSION_MINIMUM setting needed on Windows for some reason.
	# TODO does this need anything else to disable unneeded dependencies?
	cd libs/openal-soft/build && env PKG_CONFIG_PATH=$(current_dir)/root/lib/pkgconfig cmake -D CMAKE_INSTALL_PREFIX=$(current_dir)/root \
		-DCMAKE_POLICY_VERSION_MINIMUM=3.5 -DALSOFT_EXAMPLES=OFF \
		..
	cd libs/openal-soft && cmake --build build
	cd libs/openal-soft && cmake --install build

ffmpeg:
	# disabling AVX-512, AVX-512ICL, XOP, FMA4, and optionally AVX2, FMA3,
	# were recommended by Claude Code for CPU compatibility.
	# for encoders/muxers we only need ogg opus and wav
	cd libs/FFmpeg && ./configure --enable-shared --disable-encoders --disable-muxers \
		--enable-encoder=opus --enable-encoder=pcm_s16le \
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
