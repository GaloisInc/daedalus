FROM haskell:8.8.3
  
COPY . /daedalus
WORKDIR /daedalus

RUN cabal update
# Use copy instead of symlink because we are root right now
RUN cabal install exe:daedalus --installdir=/usr/local/bin --install-method=copy


