FROM ubuntu:23.10 AS build

RUN apt-get update && \
    apt-get install -y \
      curl wget git libsecret-1-0 libnuma1 g++ \
      cmake antlr4 libantlr4-runtime-dev happy libgtest-dev vim \
      hyperfine

# Install kaitai_struct_compiler
WORKDIR PLDI/Downloads
RUN wget https://github.com/kaitai-io/kaitai_struct_compiler/releases/download/0.10/kaitai-struct-compiler_0.10_all.deb
RUN dpkg -i kaitai-struct-compiler_0.10_all.deb

# Setup Haskell
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"

# Checkout and build Daedalus
WORKDIR /PLDI
## Hack to make sure we fetch if the head changes
ADD https://api.github.com/repos/GaloisInc/daedalus/git/refs/heads/master version.unused.json
RUN git clone https://github.com/GaloisInc/daedalus

RUN apt-get install -y libgmp-dev

WORKDIR daedalus
RUN cabal update && cabal build && cabal build daedalus-language-server
RUN cp $(cabal exec which daedalus) /usr/local/bin
RUN cp $(cabal exec which daedalus-language-server) /usr/local/bin

# Install VS code client code
WORKDIR syntax-highlight/vscode-daedalus/

# Note that these steps install Node from the upstream PPA rather than
# using the version provided by the Ubuntu image, which is too old.
# Using the older version results in a build failure when building the
# VScode extension.
RUN curl -sL https://deb.nodesource.com/setup_20.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install -y nodejs
RUN npm install vsce && npm install && npm run package
RUN cp *.vsix /usr/local/lib

ENV PATH="/usr/local/bin:${PATH}"

WORKDIR /PLDI/daedalus/formats/pdf/new/c++
RUN apt-get install -y pkg-config libssl-dev zlib1g-dev
RUN cmake -B build && cmake --build build

WORKDIR /PLDI/daedalus
RUN ./run-benchmarks build

RUN cp /PLDI/daedalus/formats/pdf/new/c++/build/pdf-driver/parser-test /PLDI/pdf-test

WORKDIR /PLDI
ADD inputs/ inputs/


