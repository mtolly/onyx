These instructions also expect the web player to be built. If you don't want to do that, just make an empty file at `../player/build/app.min.js`.

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

5. `make mac`

6. Program is packaged as `Onyx.app`.

# Mac - High Sierra hack

To build on macOS High Sierra as I am doing currently, the following steps must be done after the first failed attempt to compile.

1. Make a folder next to `haskell` (so, in the repo root) called `c-lib`

2. Find this folder: `~/.stack/snapshots/x86_64-osx/(random hash)/(ghc version)/lib/x86_64-osx-ghc-(ghc version)/fltkhs-(version)-(random hash)/`

3. From there, copy `libfltkc-dyn.dylib` to both of these:

  * `haskell/libfltkc.dylib`

  * `c-lib/libfltkc-dyn.dylib`

4. `stack build --extra-lib-dirs=.`

5. Comment out the stack build line in the `Makefile` under `make mac`, then run `make mac`

# Linux - Docker

## Requirements

  * `stack` (latest stable version)
  * Docker

## Steps

1. `make docker`

That's everything! This will create a Docker image (based on Ubuntu 14.04) with all library and tool dependencies, build via Stack inside that, and then also create the AppImage inside it via `linuxdeployqt`.

Note, Stack needs to be a stable version so it can download the Docker-compatible build for itself; otherwise [you might get this](https://github.com/commercialhaskell/stack/issues/4850#issuecomment-606171268).

# Linux/Mac command line installation

## Requirements

  * `stack` (latest version)
  * Various C/C++ libraries: check the Makefile for details

## Steps

1. Install dependency libraries. The Makefile contains commands for `mac-deps`, `ubuntu-deps`, and `arch-deps`, but these may or may not be up-to-date

2. `stack install` - installs to `~/.local/bin/onyx`, link or add to your PATH as you like

3. `make install-resources` - this will install a resources folder in `~/.local/bin` next to where `onyx` was placed

4. `onyx`
