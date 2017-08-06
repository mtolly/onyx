# Windows

## Requirements

  * `stack` (32-bit version)
  * Visual C++ 2008 Runtime
  * `make` (`stack exec -- pacman -Sy make`)

## Steps

1. `stack exec make win-deps`

2. Find your Stack MSYS2 mingw32 directory, something like: `C:\Users\YourNameHere\AppData\Local\Programs\stack\i386-windows\msys2-20150512\mingw32`

3. In there, remove (or move somewhere else) all `.a` files under `lib/`, except for `libSDL2main.a`.

4. Also in there, copy all `.dll` files from `bin/` to `lib/` and make the following renames:

  * `libmp3lame-0.dll` to `libmp3lame.dll`
  * `libsndfile-1.dll` to `libsndfile.dll`
  * `libsamplerate-0.dll` to `libsamplerate.dll`
  * `librubberband-2.dll` to `librubberband.dll`

5. (not currently required) Do the following to install `libbotan`.

  * Download source for `botan 2.1.0`
  * Modify `src/build-data/os/mingw.txt`: replace `building_shared_supported no` with `soname_pattern_base "libbotan-2.dll"`
  * From within stack's msys2 bash: `./configure.py --os=mingw --cpu=x86 --link-method=copy`
  * In the `Makefile`, remove the lines `$(LN) $(SONAME_PATCH) ./$(SONAME_ABI)` and `$(LN) $(SONAME_PATCH) ./$(SONAME_BASE)`
  * From within stack's msys2 bash: `make`
  * Copy `libbotan-2.dll` to Stack's `/mingw32/bin` and `/mingw32/lib`
  * Copy `build/include/botan` to Stack's `/mingw32/include`
  * Make the following `/mingw32/lib/pkgconfig/botan-2.pc`:

        prefix=/mingw32
        exec_prefix=${prefix}
        libdir=${exec_prefix}/lib
        includedir=${prefix}/include

        Name: Botan
        Description: Crypto and TLS for C++11
        Version: 2.1.0

        Libs: -L${libdir} -lbotan-2 -fstack-protector -pthread
        Libs.private: -ladvapi32
        Cflags: -I${includedir}

6. `stack build` (do this from outside `bash` so the lib+include paths are set up right)

7. `stack exec make win`

8. Program and DLLs will now be in `win/`.

# Mac

## Requirements

  * `stack`
  * Xcode dev tools
  * Wine
  * Homebrew

## Steps

1. `make mac-deps`

2. `stack build`

3. `make mac`

4. Program is packaged as `Onyx.app`.
