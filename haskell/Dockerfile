FROM ubuntu:14.04
COPY Makefile .
RUN apt-get update && apt-get -y install make && make ubuntu-deps
# extract linuxdeployqt AppImage due to no FUSE in docker
# the sed hack comes from https://github.com/AppImage/AppImageKit/issues/828
RUN apt-get update && apt-get -y install wget
RUN wget https://github.com/probonopd/linuxdeployqt/releases/download/7/linuxdeployqt-7-x86_64.AppImage -O linuxdeployqt && chmod +x linuxdeployqt && sed '0,/AI\x02/{s|AI\x02|\x00\x00\x00|}' -i linuxdeployqt && ./linuxdeployqt --appimage-extract && ln -s $(pwd)/squashfs-root/AppRun /usr/local/bin/linuxdeployqt
# needed for final Haskell linking
# (these are installed by stack script but we're not doing that)
RUN apt-get update && apt-get -y install g++ libgmp-dev
# needed for Stack to unpack GHC
RUN apt-get update && apt-get -y install xz-utils
