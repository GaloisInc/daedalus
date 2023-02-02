
# The daedalus dockerfile.
#
# Note: the artifacts published in this image are used by the
# 'try-daedalus' project at
#
# https://github.com/GaloisInc/try-daedalus
#
# so be sure to keep that in mind if you make changes here and update
# the try-daedalus '.devcontainer.json' file as needed.

# Do the build itself in one image, then copy the important build
# artifacts to a second image that we'll actually distribute.
# FROM haskell:8.10 AS build
FROM haskell:8.10 AS build

# Hack to make sure we fetch if the head changes
ADD https://api.github.com/repos/GaloisInc/daedalus/git/refs/heads/master version.unused.json
RUN git clone https://github.com/GaloisInc/daedalus
WORKDIR daedalus

RUN cabal update

RUN cabal install exe:daedalus
RUN cabal install exe:daedalus-language-server

WORKDIR syntax-highlight/vscode-daedalus/

RUN apt-get update && apt-get install -y libsecret-1-0

# Note that these steps install Node from the upstream PPA rather than
# using the version provided by the Ubuntu image, which is too old.
# Using the older version results in a build failure when building the
# VScode extension.
RUN curl -sL https://deb.nodesource.com/setup_19.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install -y nodejs

RUN npm install vsce && npm install && npm run package

# Second image: the actual distribution image containing the user-facing
# build artifacts
FROM ubuntu:latest

WORKDIR /usr/local/

COPY --from=build /root/.cabal/bin/daedalus-language-server bin/
COPY --from=build /root/.cabal/bin/daedalus                 bin/
COPY --from=build daedalus/syntax-highlight/vscode-daedalus/*.vsix lib/

ENV PATH="/usr/local/bin/:${PATH}"

RUN apt-get update && apt-get install -y git libnuma1

WORKDIR /
