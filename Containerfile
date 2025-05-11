FROM debian:12

ARG RMTLD3SYNTH_VERSION="v0.5-x" # this will be replaced by the build environment

RUN \
  apt update && apt install -y \
  build-essential \
  software-properties-common \
  git \
  wget \
  vim \
  libgmp-dev \
  m4 \
  python3 \
  python3-distutils \
  dos2unix \
  opam \
  pkg-config

RUN opam init --disable-sandboxing -ya
RUN opam switch create 4.14.1
RUN opam pin add dolmen https://github.com/Gbury/dolmen.git#e81b130ac0fdcd7e2b08603648c54c8ead8fbd7b -y
RUN opam pin add dolmen-export https://github.com/Gbury/dolmen.git#e81b130ac0fdcd7e2b08603648c54c8ead8fbd7b -y
RUN opam install z3=4.12.3 -v -y
RUN opam pin add rmtld3synth https://github.com/anmaped/rmtld3synth.git#${RMTLD3SYNTH_VERSION} -y
ENTRYPOINT opam exec /bin/bash
WORKDIR /root