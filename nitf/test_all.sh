#!/bin/bash

# Examples
# ./test_all.sh
# ./test_all.sh
# ./test_all.sh --count --gen
# ./test_all.sh --gwg


TMPFILE="/dev/null"
USING_GEN=""
DAEDALUS="cabal run ../:daedalus --"
DAEDALUS="../nitf_cpp_parser/parser"
NITF_HAMMER=""

#NITF_TESTSUITE=`echo ~/SafeDocs/NITF`
JITC_DIR="JITC - NITF Test Data - Set 1"
JITC_DIR="/tmp/JITC - NITF Test Data - Set 1"
GWG_DIR=./gwg.nga.mil_samples


# echo "args are:"
# echo "$@"

for arg in "$@"
do
    case $arg in
        "--gen" )
            USING_GEN="--gen";;
        "--count" )
            TMPFILE=/tmp/nitf_test_output.txt
            COUNT="True"
            ;;
        "--gwg" )
            GWG="True"
            ;;
        *)
            echo "Unkown argument: ${arg}"
            exit 1
            ;;
     esac
done

test_jitc(){
    # This approach taken from http://mywiki.wooledge.org/BashFAQ/001
    printf "Positive tests (should pass):\n"
    ls -d "${JITC_DIR}"/*/POS |
    while read -r dir; do
        find "$dir" -type f -iname '*.ntf' -o -iname '*.nitf' |
        while read -r file; do
            printf ' %s ... ' "$file"
            # if ${DAEDALUS} "$file" > ${TMPFILE}; then
            # if ${NITF_HAMMER} -f "$file" > ${TMPFILE}; then
            if cabal run ../:daedalus -- nitf_main.ddl -i"${file}" ${USING_GEN} > ${TMPFILE}; then

                printf "pass\n"
                if [ ! -z "$COUNT" ];
                then
                    cat ${TMPFILE} | head -n 2 | tail -n 1
                fi
            else
                printf "fail\n"
            fi
        done
    done

    printf "\nNegative tests (should fail):\n"
    ls -d "${JITC_DIR}"/*/NEG |
    while read -r dir; do
        find "$dir" -type f -iname '*.ntf' -o -iname '*.nitf' |
        while read -r file; do
            printf ' %s ... ' "$file"
            # if ${DAEDALUS} "$file" > ${TMPFILE}; then
            # if ${NITF_HAMMER} -f "$file" > ${TMPFILE}; then
            if cabal run ../:daedalus --  nitf_main.ddl -i"$file" ${USING_GEN} > ${TMPFILE}; then
                printf "pass\n"
                if [ ! -z "$COUNT" ];
                then
                    cat ${TMPFILE} | head -n 2 | tail -n 1
                fi
            else
                printf "fail\n"
            fi
        done
    done
}

test_gwg(){
    find "${GWG_DIR}" -type f -iname '*.ntf' -o -iname '*.nitf' |
    while read -r file; do
        printf '%s ... ' "$file"
        # if ${DAEDALUS} "$file" > ${TMPFILE}; then
        if cabal run ../:daedalus -- nitf_main.ddl -i"$file" ${USING_GEN} > ${TMPFILE}; then
            printf "pass\n"
            if [ ! -z "$COUNT" ];
            then
                cat ${TMPFILE} | head -n 2 | tail -n 1
            fi
        else
            printf "fail\n"
        fi
    done
}

if [ -z "$GWG" ]; then
    test_jitc
else
    test_gwg
fi
