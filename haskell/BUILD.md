Supported platforms (all 64-bit), others may work but require extra setup:

  * Windows + MSYS2
  * macOS
  * Ubuntu Linux, including via Docker

Skip to the Docker section at the bottom for Docker steps.

# Pre-build requirements

  * [`stack`](https://haskellstack.org/)

## Windows

  * [NSIS](http://nsis.sourceforge.net/Main_Page)

On Windows, [Git Bash](https://gitforwindows.org/) is recommended to run shell commands.

After installing `stack`, make sure MSYS2 is updated with:

1. `stack exec -- pacman -Syy`

2. `stack exec -- pacman -Syu` (then press `y` to exit)

3. `stack exec -- pacman -Syu` again

Open Git Bash and execute the following commands:

  *  git config --global core.autocrlf false
  *  git config --global core.eol lf
  
These commands ensure that when the repo is pulled, there are no formatting issues with end-of-line characters. An issue with the build dependencies can appear if this step is skipped.

Using `cd`, navigate to the folder you wish to download the Onyx repo to

Finally, pull the repo using Git Bash (and **not** GitHub GUI) with the following command:
  *  git clone https://github.com/mtolly/onyx.git

## macOS

  * Xcode dev tools
  * [Homebrew](https://brew.sh/)

## Linux

  * [`linuxdeploy`](https://github.com/linuxdeploy/linuxdeploy)

# Build C dependencies

1. `git submodule update --init`

2. `./pre-dependencies` (add `sudo` if necessary)

3. `./build-dependencies`

# Build Onyx

1. `./stack-local build`

2. `./copy-resources`

3. `./package`

Your OS-specific package should be created.

# Docker

1. Install Docker (or, install Podman and edit `build-docker`)

2. `./build-docker`

3. AppImage will be created

# Command line installation

Follow normal build instructions, but instead of `./package`,
run `./install-cli <DIR>` to install `onyx` and `onyx-files` into `<DIR>`.
Default installation folder is `~/.local/bin`.
