#!/bin/bash

# install and build the daedalus-language-server, along with deps

# deps.
# build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# install ghc
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=8.8.4
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

export PATH=$HOME/.ghcup/bin:$PATH

# build and install the server

cabal install --overwrite-policy=always  daedalus-language-server


# VSCode extension
pushd syntax-highlight/vscode-daedalus > /dev/null
npm install
npm install vsce 
npm run package
popd > /dev/null

