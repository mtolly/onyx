Supported platforms (all 64-bit), others may work but require extra setup:

  * Windows + MSYS2
  * macOS
  * Ubuntu Linux, including via Docker
  * Arch/Manjaro Linux

Skip to the Docker section at the bottom for Docker steps.

# Pre-build requirements

  * [`stack`](https://haskellstack.org/)

## Windows

  * [NSIS](http://nsis.sourceforge.net/Main_Page)

After installing `stack`, make sure MSYS2 is updated with:

1. `stack exec -- pacman -Syy`

2. `stack exec -- pacman -Syu` (then press `y` to exit)

3. `stack exec -- pacman -Syu` again

## macOS

  * Xcode dev tools
  * [Homebrew](https://brew.sh/)

## Linux

  * [`linuxdeployqt`](https://github.com/probonopd/linuxdeployqt)

# Build C dependencies

1. `git submodule init && git submodule update`

2. `./pre-dependencies` (add `sudo` if necessary)

3. `./build-dependencies`

# Build Onyx

1. `./stack-local build`

2. `./package`

Your OS-specific package should be created.

Note that `./package` will fail on too-recent Linux systems due to `linuxdeployqt`.
Instead, use either the Docker steps or command line installation.

# Docker

1. Install Docker

2. `docker build -t onyxite/onyx .`

3. AppImage will be located at `/onyx/Onyx-*-x86_64.AppImage` in the image

# Command line installation

Follow normal build instructions, but instead of `./package`,
run `./install-cli <DIR>` to install `onyx` and `onyx-files` into `<DIR>`.
Default installation folder is `~/.local/bin`.
