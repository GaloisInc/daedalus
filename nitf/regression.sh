#!/bin/bash

FILENAME=regression_nitf.txt
OUTPUT=/tmp/${FILENAME}
WHICH_NITF=""
NITF_MODERN="--nitf-modern"

for arg in "$@"
do
    case $arg in
        "--init" )
            OUTPUT=${FILENAME}
            ISINIT="True"
            NITF_MODERN=""
            ;;
    esac
done


./test_all.sh --count ${WHICH_NITF} ${NITF_MODERN} | tee ${OUTPUT}
./test_all.sh --count ${WHICH_NITF} ${NITF_MODERN} --gwg | tee -a ${OUTPUT}

if [ -z ${ISINIT} ] ; then
    diff $FILENAME /tmp/$FILENAME
fi
