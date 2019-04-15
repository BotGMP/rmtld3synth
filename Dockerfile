FROM ubuntu:18.04

RUN \
  apt update && apt upgrade -y && \
  apt install -y \
  build-essential \
  software-properties-common \
  git \
  wget \
  vim \
  libgmp-dev \
  m4

RUN add-apt-repository ppa:avsm/ppa && apt update && apt install opam -y

RUN opam init --disable-sandboxing -ya
RUN opam pin add dolmen https://github.com/Gbury/dolmen.git#b4adc6044db0912ffa7cc9e38917cad85465d572 -y
RUN opam pin add dolmen-export https://github.com/Gbury/dolmen.git#b4adc6044db0912ffa7cc9e38917cad85465d572 -y
RUN apt install python -y && opam install z3 -v -y
RUN opam pin add rmtld3synth https://github.com/anmaped/rmtld3synth.git -y
RUN eval $(opam env); cp $(ocamlfind query z3)/libz3.so /usr/lib
ENTRYPOINT opam config exec /bin/bash
WORKDIR /root
