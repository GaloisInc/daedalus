#!/bin/bash

BIN=$1.fixed

cp $1 $BIN

cabal exec daedalus -- run --json --entry=Fixups nitf/nitf-fixup.ddl -i $BIN | \
    jq '.[] | .[] | (.offset,.nbytes,.size)' | \
    xargs -n 3 | \
    while read a b c; \
    do dd conv=notrunc oseek=$a of=$BIN bs=1 count=$b if=<(printf "%.${b}d" $c); \
    done     


