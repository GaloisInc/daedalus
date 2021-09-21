#!/bin/bash

# To update the regression_nitf.txt file do
# ./regression.sh --init

# To run the tests and see the diff do
# ./regression.sh
# ./regression.sh --gen   ## using the parser-gen interpreter

FILENAME=regression_nitf.txt
OUTPUT=/tmp/${FILENAME}
WHICH_NITF=""
NITF_MODERN="--nitf-modern"
GEN=""


for arg in "$@"
do
    case $arg in
        "--init" )
            OUTPUT=${FILENAME}
            ISINIT="True"
            NITF_MODERN=""
            ;;
        "--gen" )
            GEN="--gen"
            ;;
        *)
            echo "Unkown argument: ${arg}"
            exit 1
            ;;
    esac
done


./test_all.sh --count ${WHICH_NITF} ${NITF_MODERN} ${GEN} | tee ${OUTPUT}
./test_all.sh --count ${WHICH_NITF} ${NITF_MODERN} ${GEN} --gwg | tee -a ${OUTPUT}

if [ -z ${ISINIT} ] ; then
    echo "DIFF:"
    diff $FILENAME /tmp/$FILENAME
fi
