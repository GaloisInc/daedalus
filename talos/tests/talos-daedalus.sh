#!/bin/bash

cabal exec talos -- -n 1 $1 | daedalus $1 --inter=-