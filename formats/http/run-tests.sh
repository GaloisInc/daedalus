#!/usr/bin/env bash

# Run the Daedalus tool to parse test cases using the HTTP specs in this
# directory and compare them to expected outputs. This attempts to use
# daedalus in the PATH; if it cannot be found there, daedalus is sought
# in dist-newstyle/ at the root of this repository. This script assumes
# that the 'ghc' in the PATH is the one that 'cabal' will use to build
# daedalus.
#
# This script runs the parser against all of the test case files
# in the tests/{requests,responses}/inputs/ subdirectory and
# compares each with the respectively-named expected output file in
# tests/{requests,responses}/outputs/. This runner supports expected
# passes and expected failures simply by way of having daedalus success
# and failure output being captured in the expected output files.
#
# Note that while it's ordinarily good practice to use set -e, it is
# deliberately skipped in this script because many intermediate steps
# need to be allowed to fail so we can continue running tests and
# collect the number of failures. Doing so with set -e is annoying to
# get right, so this script is carefully written to cope without it.

HERE=$(cd `dirname $0`; pwd)

# The repo root relative to this script's location.
ROOT=$HERE/../../

# in_path <PROG>; exit code is zero if the specified program is found in
# the PATH. Produces no output.
function in_path {
    local cmd=$1
    which $cmd 2>&1 >/dev/null
}

function ghc_version {
    ghc --version | awk '{ print $NF }'
}

# Echo the path to the daedalus binary, either in the PATH or in
# dist-newstyle in the repo root.
function find_daedalus {
    which daedalus || {
        ghc_ver=$(ghc_version)
        find $ROOT/dist-newstyle -type f -name daedalus 2>/dev/null | grep $ghc_ver
    }
}

# parse <SPEC> <ENTRY> <INPUT_PATH> <EXPECTED_OUTPUT_PATH>; run Daedalus
# using the specified spec using the entry point ENTRY on the specified
# input file and compare the results to the contents of the specified
# expected output file.
function run_test_case {
    local spec=$1
    local entry=$2
    local input_file=$3
    local output_file=$4

    args="run $spec --entry=$entry -i $input_file"

    if [ -f "${output_file}" ]
    then
        tmpfile=$(mktemp)

        $DAEDALUS $args > ${tmpfile}

        if diff --color $output_file $tmpfile
        then
            echo "  PASS      match: $output_file"
            return 0
        else
            echo "  FAIL      mismatch: $output_file"
            return 1
        fi

        rm $tmpfile
    else
        echo "  Generated new expected output at ${output_file}"
        $DAEDALUS $args > ${output_file}
        return 0
    fi
}

# run_test_group <SPEC> <TESTS_DIR> <ENTRY_POINT>
#
# Run all test cases in the specified directory using the specified
# spec entry point. Expects a test directory with subdirectories
# inputs/ and outputs/. Mutates two global counters: num_failures and
# num_successes.
function run_test_group {
    local spec=$1
    local test_case_dir=$2
    local entry_point=$3

    for file in $test_case_dir/inputs/*.txt
    do
        filename=$(basename $file)
        output_file="$test_case_dir/outputs/${filename}"

        echo ${file}:
        run_test_case $spec $entry_point $file $output_file
        num_failures=$((num_failures + $?))
        num_successes=$((num_successes + (1 - $?)))
        echo
    done
}

DAEDALUS=$(find_daedalus)

if [ -z "$DAEDALUS" ]
then
    echo "Error: could not find daedalus in the PATH or in dist-newstyle/ in the repo for GHC version $(ghc_version)."
    exit 1
fi

# Canonicalize the path and remove relative path segments
DAEDALUS=$(readlink -f $DAEDALUS)

echo "Using daedalus at:"
echo "  $DAEDALUS"
echo

num_failures=0
num_successes=0

cd $HERE

HTTP_1_SPEC=$HERE/HTTP-1.1.ddl
run_test_group $HTTP_1_SPEC tests/http1/requests HTTP_request
run_test_group $HTTP_1_SPEC tests/http1/responses HTTP_status

echo "$num_successes tests passed, $num_failures tests failed."

exit $num_failures
