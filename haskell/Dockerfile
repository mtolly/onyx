FROM ubuntu:16.04

# probably only needed in later ubuntu versions
ENV TZ=America/Chicago
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# extract linuxdeployqt AppImage due to no FUSE in docker
# the sed hack comes from https://github.com/AppImage/AppImageKit/issues/828
RUN apt-get update && apt-get -y install wget
RUN wget https://github.com/probonopd/linuxdeployqt/releases/download/7/linuxdeployqt-7-x86_64.AppImage -O linuxdeployqt && chmod +x linuxdeployqt && sed '0,/AI\x02/{s|AI\x02|\x00\x00\x00|}' -i linuxdeployqt && ./linuxdeployqt --appimage-extract && ln -s $(pwd)/squashfs-root/AppRun /usr/local/bin/linuxdeployqt
# needed by linuxdeployqt
RUN apt-get update && apt-get -y install file libglib2.0-0

COPY pre-dependencies /onyx/pre-dependencies
RUN /onyx/pre-dependencies

COPY dependencies /onyx/dependencies
RUN cd /onyx/dependencies && make

RUN wget -qO- https://get.haskellstack.org/ | sh

COPY . /onyx
RUN cd /onyx && ./stack-local setup
RUN cd /onyx && ./stack-local build
RUN cd /onyx && ./package
