#!/usr/bin/env bash

# Run the Daedalus tool to parse test cases using the HTTP specs in this
# directory and compare them to expected outputs. This attempts to use
# daedalus in the PATH; if it cannot be found there, daedalus is sought
# in dist-newstyle/ at the root of this repository. This script assumes
# that the 'ghc' in the PATH is the one that 'cabal' will use to build
# daedalus.
#
# This script requires that "xxd" is installed in the PATH and that the
# version of GHC that cabal used to build daedalus is also in the PATH.
#
# For HTTP 1.1, this script runs the parser against all of the test case
# files in the tests/http1/{requests,responses}/inputs/ subdirectory
# and compares each with the respectively-named expected output file
# in tests/http1/{requests,responses}/outputs/. Since HTTP 1.1 is a
# text-based protocol, the test case input files are plain text files.
#
# For HTTP 2, this script runs the parser against all of the test
# case files in the tests/http2/frames/inputs/ subdirectory and
# compares each with the respectively-named expected output file in
# tests/http2/frames/outputs/. Since HTTP 2 is a binary protocol,
# each test case is a text file in the format produced by "xxd", with
# extension ".xxd", for easier user editing. Each ".xxd" file will be
# fed to "xxd" to generate a binary file for input to the parser as part
# of the testing process.
#
# This runner supports expected passes and expected failures simply by
# way of having daedalus success and failure output being captured in
# the expected output files.
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
    ghc --version 2>/dev/null | awk '{ print $NF }'
}

# Echo the path to the daedalus binary, either in the PATH or in
# dist-newstyle in the repo root.
function find_daedalus {
    which daedalus || {
        ghc_ver=$(ghc_version)
        if [ ! -z "$ghc_ver" ]
        then
            find $ROOT/dist-newstyle -type f -name daedalus 2>/dev/null | grep $ghc_ver
        fi
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

# run_test_group <SPEC> <INPUT FILE TYPE> <TESTS_DIR> <ENTRY_POINT>
#
# Run all test cases in the specified directory using the specified
# spec (DDL) file and entry point. Expects a test directory with
# subdirectories inputs/ and outputs/. Mutates two global counters:
# num_failures and num_successes. The names of the input files in
# inputs/ will depend on the input file type:
#
# INPUT FILE TYPE can be either "text" and "binary". If "text",
# input test case files are treated as text files and expected to
# end in ".txt"; if "binary", input test case files are treated as
# "xxd"-compatible encodings of binary files and are expected to end in
# ".xxd". In the latter case, each test case file will be fed to "xxd"
# to construct a binary input file for parsing as part of the testing
# process.
function run_test_group {
    local spec=$1
    local ty=$2
    local test_case_dir=$3
    local entry_point=$4

    binary=0
    if [ "$ty" == "text" ]
    then
        files=$test_case_dir/inputs/*.txt
    elif [ "$ty" == "binary" ]
    then
        binary=1
        files=$test_case_dir/inputs/*.xxd
    else
        echo "Error: invalid test case type $ty"
        exit 1
    fi

    for file in $files
    do
        filename=$(basename $file)
        output_file="$test_case_dir/outputs/${filename}"

        local input_file=$file
        if [ $binary -eq 1 ]
        then
            # It's critical that we always re-use the same input file
            # path here so that paths included in error messages match
            # the expected output. (Previously this created a temp file
            # for each test case but that mean the paths changed with
            # every run, which breaks this.)
            input_file=/tmp/input_file.bin
            xxd -r $file > $input_file
        fi

        echo ${file}:
        run_test_case $spec $entry_point $input_file $output_file
        num_failures=$((num_failures + $?))
        num_successes=$((num_successes + (1 - $?)))
        echo
    done
}

if [ -z "$(ghc_version)" ]
then
    echo "Error: 'ghc' not found in the PATH, exiting"
    exit 1
fi

DAEDALUS=$(find_daedalus)

if [ -z "$DAEDALUS" ]
then
    echo "Error: could not find daedalus in the PATH or in dist-newstyle/ in the repo for GHC version $(ghc_version)."
    exit 1
fi

if ! in_path xxd
then
    echo "Error: 'xxd' not in the PATH; xxd is required to run the test suite.'"
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
run_test_group $HTTP_1_SPEC text tests/http1/requests HTTP_request
run_test_group $HTTP_1_SPEC text tests/http1/responses HTTP_status

HTTP_2_SPEC=$HERE/HTTP-2.ddl
run_test_group $HTTP_2_SPEC binary tests/http2/frames HTTP2_frame

echo "$num_successes tests passed, $num_failures tests failed."

exit $num_failures
