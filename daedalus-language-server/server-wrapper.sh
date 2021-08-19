#!/bin/bash

# requires env variable DAEDALUS to point to the root directory of
# DaeDaLus

cd $DAEDALUS
cabal exec daedalus-language-server

