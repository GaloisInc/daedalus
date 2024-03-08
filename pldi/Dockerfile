FROM haskell:9.4 AS build

RUN apt-get update && apt-get install -y wget git libsecret-1-0 libnuma1 g++ cmake antlr4 happy

# Install kaitai_struct_compiler
WORKDIR PLDI/Downloads
RUN wget https://github.com/kaitai-io/kaitai_struct_compiler/releases/download/0.10/kaitai-struct-compiler_0.10_all.deb
RUN dpkg -i kaitai-struct-compiler_0.10_all.deb

## Hack to make sure we fetch if the head changes
ADD https://api.github.com/repos/GaloisInc/daedalus/git/refs/heads/master version.unused.json
RUN git clone https://github.com/GaloisInc/daedalus

WORKDIR daedalus
RUN cabal update
RUN cabal install --install-method=copy --installdir=/usr/local/bin exe:daedalus
RUN cabal install --install-method=copy --installdir=/usr/local/bin exe:daedalus-language-server
ENV PATH="/usr/local/bin:${PATH}"

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
COPY *.vsix /usr/local/lib

ENV PATH="/usr/local/bin:${PATH}"

WORKDIR /PLDI/Downloads
RUN wget https://github.com/sharkdp/hyperfine/releases/download/v1.18.0/hyperfine_1.18.0_amd64.deb && dpkg -i hyperfine_1.18.0_amd64.deb

WORKDIR /PLDI
ADD run-benchmarks .
