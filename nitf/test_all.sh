#!/bin/bash


TMPFILE="/dev/null"
NITF_FOLDER=""
USING_GEN=""
DAEDALUS="cabal run ../:daedalus --"
DAEDALUS="/daedalus/nitf_cpp_parser/parser"

GWG_DIR=./gwg.nga.mil_samples

# echo "args are:"
# echo "$@"

for arg in "$@"
do
    case $arg in
        "--gen" )
            USING_GEN="--gen";;
        "--nitf-modern" )
            NITF_FOLDER="nitf-modern/";;
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
    ls -d JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1/*/POS |
    while read -r dir; do
        find "$dir" -type f -iname '*.ntf' -o -iname '*.nitf' |
        while read -r file; do
            printf ' %s ... ' "$file"
            #if ${DAEDALUS} "$file" > ${TMPFILE}; then
            if cabal run ../:daedalus -- ${NITF_FOLDER}nitf_main.ddl -i"$file" ${USING_GEN} > ${TMPFILE}; then

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
    ls -d JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1/*/NEG |
    while read -r dir; do
        find "$dir" -type f -iname '*.ntf' -o -iname '*.nitf' |
        while read -r file; do
            printf ' %s ... ' "$file"
            #if ${DAEDALUS} "$file" > ${TMPFILE}; then
            if cabal run ../:daedalus --  ${NITF_FOLDER}nitf_main.ddl -i"$file" ${USING_GEN} > ${TMPFILE}; then
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
        if cabal run ../:daedalus -- nitf-modern/nitf_main.ddl -i"$file" > ${TMPFILE}; then
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
