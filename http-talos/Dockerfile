
# The daedalus dockerfile.

# Do the build itself in one image, then copy the important build
# artifacts to a second image that we'll actually distribute.
# FROM haskell:8.10 AS build
FROM haskell:9.4 AS build

# Hack to make sure we fetch if the head changes
ADD https://api.github.com/repos/GaloisInc/daedalus/git/refs/heads/wip/talos-http-docker version.unused.json
RUN git clone -b wip/talos-http-docker --single-branch https://github.com/GaloisInc/daedalus
WORKDIR daedalus

RUN cabal update

RUN cabal install exe:daedalus
RUN cabal install exe:talos

# Second image: the actual distribution image containing the user-facing
# build artifacts.  We needa more recent ubuntu to get a decent z3
FROM ubuntu:jammy

WORKDIR /usr/local/

COPY --from=build /root/.cabal/bin/daedalus                 bin/
COPY --from=build /root/.cabal/bin/talos                    bin/

WORKDIR /
COPY --from=build /daedalus/http-talos                     /http/

ENV PATH="/usr/local/bin/:${PATH}"

RUN apt-get update && apt-get install -y git libnuma1 z3

WORKDIR /http
