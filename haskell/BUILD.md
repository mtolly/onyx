# Windows

## Requirements

  * `stack` (64-bit version)
  * Visual C++ 2008 Runtime
  * .NET Framework
  * `make` (`stack exec -- pacman -Sy make`)
  * [NSIS](http://nsis.sourceforge.net/Main_Page)

## Steps

1. `stack exec make win-deps`

2. Find your Stack MSYS2 mingw64 directory, something like: `C:\Users\YourNameHere\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\mingw64`

3. In there, remove (or move somewhere else) all `.a` files under `lib/`, except for `libSDL2main.a`.

4. Also in there, copy all `.dll` files from `bin/` to `lib/` and make the following renames:

  * `libmp3lame-0.dll` to `libmp3lame.dll`
  * `libsndfile-1.dll` to `libsndfile.dll`
  * `libsamplerate-0.dll` to `libsamplerate.dll`
  * `librubberband-2.dll` to `librubberband.dll`

5. `stack build` (do this from outside `bash` so the lib+include paths are set up right)

6. `stack exec make win`

7. Setup program will be created.

# Mac

## Requirements

  * `stack`
  * Xcode dev tools
  * Homebrew

## Steps

1. `make mac-deps` (if you want to use an existing Wine or Mono, edit the `Makefile`)

2. `stack build`

3. `make mac`

4. Program is packaged as `Onyx.app`.
