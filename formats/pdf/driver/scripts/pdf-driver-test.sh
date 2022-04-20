#!/bin/bash

MY_PATH=$(dirname "$0")
TEST_DIR=$MY_PATH/../test

if [ $# == 1 -a \( "$1" = "quick" -o "$1" = "long" \) ] ; then
  TARGET=$1
else
  echo "Usage: pdf-driver-test.sh {quick,long}"
  exit 1
fi

cd $TEST_DIR

RUNTESTSET_TIMEOUT_IN_SECS=600 make $TARGET-status

if [ $? -eq 0 ]
then
    echo
    echo pdf-hs-driver $TARGET tests GOOD: all variances accounted for.
    exit 0
else
    echo
    echo pdf-hs-driver $TARGET tests ERROR: unaccounted for variances.
    exit 1
fi
