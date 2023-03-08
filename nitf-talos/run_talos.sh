#!/bin/bash

OUT=/nitf-out/

if [ $# -lt 1 ];
then
    echo "Usage: run_talos.sh COUNT"
    exit 1
fi

COUNT=$1

mkdir -p out
rm -f out/nitf.bin.*

rm -f nitf/talos_lib.ddl
ln -s nitf/talos_lib_synth.ddl nitf/talos_lib.ddl

/usr/local/bin/talos -n $COUNT -O out/nitf.bin -t 'pathsymb max-depth=10 num-models=1000' -a fields -i nitf/nitf_inverses.ddl nitf/nitf_main.ddl > /dev/null

rm nitf/talos_lib.ddl
ln -s nitf/talos_lib_fixup.ddl nitf/talos_lib.ddl

for i in out/nitf.bin.*;
do
    ./fixup-nitf.sh $i
done  > /dev/null 2>&1;

for i in out/nitf.bin.*.fixed;
do
    f=$(basename $i .fixed)
    cp $i $OUT/$f
done > /dev/null

