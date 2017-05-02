# Windows

## Requirements

  * `stack` (32-bit version)
  * .NET Framework v3.5
  * Visual C++ 2008 Runtime
  * `make` (`stack exec -- pacman -Sy make`)

## Steps

1. `stack exec make win-deps`

2. Find your Stack MSYS2 mingw32 directory, something like: `C:\Users\YourNameHere\AppData\Local\Programs\stack\i386-windows\msys2-20150512\mingw32`

3. In there, remove (or move somewhere else) all `.a` files under `lib/`, except for `libSDL2main.a`.

4. Also in there, copy all `.dll` files from `bin/` to `lib/` and remove the number suffixes in the `lib/` versions (so e.g. copy `bin/libmp3lame-0.dll` to `lib/libmp3lame.dll`)

5. `stack build` (do this from outside `bash` so the lib+include paths are set up right)

6. `stack exec make win`

7. Program and DLLs will now be in `win/`.

# Mac

## Requirements

  * `stack`
  * Xcode dev tools
  * Mono
  * Wine
  * Homebrew

## Steps

1. `make mac-deps`

2. `stack build`

3. `make mac`

4. Program is packaged as `Onyx.app`.
