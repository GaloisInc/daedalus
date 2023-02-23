#!/usr/bin/env bash

# Run the Daedalus tool to parse test cases using the HTTP spec and
# compare them to expected outputs. This attempts to use daedalus
# in the PATH; if it cannot be found there, daedalus is sought in
# dist-newstyle/ at the root of this repository.
#
# This script runs the parser against all of the test case files
# in the tests/inputs/ subdirectory and compares each with the
# respectively-named expected output file in tests/outputs/.

set -e

HERE=$(cd `dirname $0`; pwd)
SPEC=$HERE/HTTP.ddl

# in_path <PROG>; exit code is zero if the specified program is found in
# the PATH. Produces no output.
function in_path {
    local cmd=$1
    which $cmd 2>&1 >/dev/null
}

# Echo the path to the daedalus binary, either in the PATH or in
# dist-newstyle in the repo root.
function find_daedalus {
    if in_path daedalus
    then
        which daedalus
    else
        ghc_ver=$(ghc --version | awk '{ print $NF }')
        root=$HERE/../../
        find $root/dist-newstyle -type f -name daedalus | grep $ghc_ver
    fi
}

# parse <ENTRY> <PATH>; run Daedalus using the spec configured in
# $SPEC using the entry point ENTRY on the specified input file. Uses
# daedalus from the PATH if possible; otherwise it uses daedalus from
# dist-newstyle at the root of the repo.
function run_test_case {
    local entry=$1
    local input_file=$2
    local output_file=$3

    args="run $SPEC --entry=$entry -i $input_file"

    cmd=$(find_daedalus)

    if [ -z "$cmd" ]
    then
        echo "Error: could not find daedalus in the PATH or in dist-newstyle/ in the repo."
        exit 1
    fi

    if [ -f "${output_file}" ]
    then
        tmpfile=$(mktemp)
        $cmd $args > ${tmpfile}

        if diff --color $output_file $tmpfile
        then
            echo "  PASSED; output matches $output_file"
        else
            echo "  FAILED; did not match output in $output_file"
        fi

        rm $tmpfile
    else
        echo "  Generated new expected output at ${output_file}"
        $cmd $args > ${output_file}
    fi
}

cd $HERE
FILES=tests/inputs/*_request*.txt

for request_file in $FILES
do
    filename=$(basename $request_file)
    output_file="tests/outputs/${filename}"

    echo ${request_file}:
    run_test_case HTTP_request $request_file $output_file
    echo
done
