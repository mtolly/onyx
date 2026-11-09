FROM ubuntu:20.04 AS build

# keeps apt from prompting (e.g. tzdata asking for a timezone);
# ARG rather than ENV so it doesn't stick around in the final image
ARG DEBIAN_FRONTEND=noninteractive

# wget + ca-certificates: downloads below
# file, libglib2.0-0: needed by linuxdeployqt, do we still need for linuxdeploy?
# gcc ... netbase: what the official Stack installer script would install
RUN apt-get update && apt-get -y install --no-install-recommends \
    wget ca-certificates file libglib2.0-0 \
    g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase

# extract linuxdeploy AppImage due to no FUSE in docker
# the sed hack comes from https://github.com/AppImage/AppImageKit/issues/828
RUN wget https://github.com/linuxdeploy/linuxdeploy/releases/download/1-alpha-20220822-1/linuxdeploy-x86_64.AppImage -O linuxdeploy && chmod +x linuxdeploy && sed '0,/AI\x02/{s|AI\x02|\x00\x00\x00|}' -i linuxdeploy && ./linuxdeploy --appimage-extract && ln -s $(pwd)/squashfs-root/AppRun /usr/local/bin/linuxdeploy

COPY pre-dependencies /onyx/pre-dependencies
RUN /onyx/pre-dependencies

COPY dependencies /onyx/dependencies
RUN cd /onyx/dependencies && make

# pinned Stack release
ARG STACK_VERSION=3.11.1
ARG STACK_SHA256=1fda71e657cd8d355625cc66b61b352699279dfee2664c014a392163bd19a952
RUN wget -q https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64.tar.gz -O stack.tar.gz \
    && echo "${STACK_SHA256}  stack.tar.gz" | sha256sum -c - \
    && tar xzf stack.tar.gz --strip-components=1 -C /usr/local/bin stack-${STACK_VERSION}-linux-x86_64/stack \
    && rm stack.tar.gz \
    && stack --version

WORKDIR /onyx

# install GHC and build Haskell dependencies before copying full source
COPY stack.yaml stack.yaml.lock stack-local LICENSE.txt ./
COPY icon icon

# install GHC outside project
RUN cd /tmp && stack --resolver "$(sed -n 's/^resolver: *\([^ #]*\).*/\1/p' /onyx/stack.yaml)" setup

# extra-deps: Stack needs full source
COPY packages/onyx-arktool packages/onyx-arktool
COPY packages/onyx-kakasi packages/onyx-kakasi
COPY packages/onyx-gamecube-dsp packages/onyx-gamecube-dsp
COPY packages/onyx-wwise packages/onyx-wwise
COPY packages/onyx-npdata packages/onyx-npdata
COPY packages/onyx-moggcrypt packages/onyx-moggcrypt
COPY packages/onyx-autochart packages/onyx-autochart
COPY packages/onyx-vorbisenc packages/onyx-vorbisenc
# onyx-moggcrypt depends on onyx-handle
COPY packages/onyx-handle packages/onyx-handle
# main packages: only manifests needed
COPY packages/onyx-lib/package.yaml packages/onyx-lib/Setup.hs packages/onyx-lib/
COPY packages/onyx-lib-game/package.yaml packages/onyx-lib-game/
COPY packages/onyx-exe-toolkit/package.yaml packages/onyx-exe-toolkit/Setup.hs packages/onyx-exe-toolkit/
COPY packages/onyx-ffmpeg/onyx-ffmpeg.cabal packages/onyx-ffmpeg/
COPY packages/onyx-fluent/onyx-fluent.cabal packages/onyx-fluent/
COPY packages/onyx-js/onyx-js.cabal packages/onyx-js/Setup.hs packages/onyx-js/

RUN ./stack-local build --only-dependencies

COPY . /onyx
RUN ./stack-local build onyx-lib
RUN ./stack-local build onyx-lib-game
RUN ./stack-local build
RUN ./package

# holds only AppImage, so `build --output` can export it to the host
FROM scratch AS export
COPY --from=build /onyx/*.AppImage /
