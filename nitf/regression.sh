#!/bin/bash

# To update the regression_nitf.txt file do
# ./regression.sh --init

# To run the tests and see the diff do
# ./regression.sh
# ./regression.sh --gen   ## using the parser-gen interpreter

set -e

FILENAME=regression_nitf.txt
OUTPUT=/tmp/${FILENAME}
GEN=""


for arg in "$@"
do
    case $arg in
        "--init" )
            OUTPUT=${FILENAME}
            ISINIT="True"
            NITF_SPEC=""
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


./test_all.sh --count ${GEN} | tee ${OUTPUT}
./test_all.sh --count ${GEN} --gwg | tee -a ${OUTPUT}

if [ -z ${ISINIT} ] ; then
    echo ""
    echo "***********    DIFF   ************"
    diff $FILENAME /tmp/$FILENAME
fi
