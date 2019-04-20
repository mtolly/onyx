# Windows

## Requirements

  * `stack` (latest version, 64-bit)
  * Visual C++ 2008 Runtime
  * .NET Framework
  * `make` (`stack exec -- pacman -Sy make`)
  * [NSIS](http://nsis.sourceforge.net/Main_Page)

## Steps

1. `stack exec make win-deps`

2. `stack build` (do this from outside `bash` so the lib+include paths are set up right)

3. `stack exec make win`

4. Setup program will be created.

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
