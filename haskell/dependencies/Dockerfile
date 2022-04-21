FROM ubuntu:21.10
ENV TZ=America/Chicago
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get -y update && apt-get -y install build-essential cmake autogen autoconf libtool libtool-bin pkg-config gettext python-is-python3 ruby nasm libx11-dev libxft-dev libgl-dev libxext-dev libglu-dev libjack-dev libpulse-dev libasound2-dev
# TODO maybe these for sdl2: libudev dbus ibus
# TODO onyx also needs libbz2-dev and libmpg123-dev
COPY extra onyx-deps/extra
COPY libs onyx-deps/libs
COPY make-pkg-config onyx-deps/make-pkg-config
COPY Makefile onyx-deps/Makefile
RUN cd onyx-deps && make
