# Windows

## Requirements

  * `stack` (latest version, 64-bit)
  * Visual C++ 2008 Runtime
  * [NSIS](http://nsis.sourceforge.net/Main_Page)

## Steps

1. `pacman -Syy` and `pacman -Syu` to make sure MSYS2 is updated

2. Install `make` with `stack exec -- pacman -Sy make`

3. `stack exec make win-deps`

4. `stack build` (do this from outside `bash` so the lib+include paths are set up right)

5. `stack exec make win`

6. Setup program will be created.

# Mac

## Requirements

  * `stack` (latest version)
  * Xcode dev tools
  * Homebrew

## Steps

1. `make mac-deps` (if you want to use an existing Wine, edit the `Makefile`)

2. Follow the instructions printed by Homebrew for setting up `openal-soft` in `pkg-config`.

3. `stack build`

4. Build the web player in `../player`. (Or make a dummy `build` folder with `app.min.js` if you don't need it.)

5. `make mac`

6. Program is packaged as `Onyx.app`.
