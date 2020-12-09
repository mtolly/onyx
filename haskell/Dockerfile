FROM ubuntu:16.04
# configure and install ffmpeg ourselves
# note: submodule is on release/4.3 branch; master currently fails with --disable-encoders option
COPY ffmpeg ffmpeg
RUN apt-get update && apt-get -y install make build-essential nasm
RUN cd ffmpeg && ./configure --enable-shared --disable-encoders && make && make install
# other dependencies
COPY Makefile .
RUN apt-get update && make ubuntu-deps
# extract linuxdeployqt AppImage due to no FUSE in docker
# the sed hack comes from https://github.com/AppImage/AppImageKit/issues/828
RUN apt-get update && apt-get -y install wget
RUN wget https://github.com/probonopd/linuxdeployqt/releases/download/7/linuxdeployqt-7-x86_64.AppImage -O linuxdeployqt && chmod +x linuxdeployqt && sed '0,/AI\x02/{s|AI\x02|\x00\x00\x00|}' -i linuxdeployqt && ./linuxdeployqt --appimage-extract && ln -s $(pwd)/squashfs-root/AppRun /usr/local/bin/linuxdeployqt
# needed for final Haskell linking
# (these are installed by stack script but we're not doing that)
RUN apt-get update && apt-get -y install g++ libgmp-dev
# needed for Stack to unpack GHC
RUN apt-get update && apt-get -y install xz-utils
# needed by linuxdeployqt, came with 14.04 but not 16.04
RUN apt-get update && apt-get -y install file
