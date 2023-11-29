#!/usr/bin/env bash

function print_help {
  echo "usage: $(basename $0) FMT1 FMT2"
  echo ""
  echo "Creates a 'stack' polyglot skeleton with FMT1 as the base (suffix) and FMT2 as the prefix.  Assumes FMT1.ddl and FMT2.ddl exist in the current directory and define FMT1Contents and FMT2Contents."
}

if [ $# -ne 2 ]; then
  print_help
fi

fmt1=${1^^}
fmt2=${2^^}

echo " {-|
  Name: $fmt1-$fmt2
  Description: This file contains a Daedalus description of a $fmt1/$fmt2
  polyglot that composes the files generated from $fmt1.ddl and $fmt2.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import $fmt1
import $fmt2

def Main =
  block
    Prefix
    Suffix

def Prefix = ${fmt2}Contents
def Suffix = ${fmt1}Contents"
