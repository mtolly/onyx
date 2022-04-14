FROM ubuntu:21.10
RUN apt update
RUN apt install build-essential cmake autogen autoconf libtool libtool-bin pkg-config gettext python-is-python3 ruby nasm libx11-dev libxft-dev libgl-dev libglu-dev
COPY extra extra
COPY libs libs
COPY make-pkg-config make-pkg-config
COPY Makefile Makefile
RUN make
