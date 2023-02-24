#!/usr/bin/env bash

# Run the Daedalus tool to parse test cases using the HTTP spec and
# compare them to expected outputs. This attempts to use daedalus
# in the PATH; if it cannot be found there, daedalus is sought in
# dist-newstyle/ at the root of this repository.
#
# This script runs the parser against all of the test case files
# in the tests/inputs/ subdirectory and compares each with the
# respectively-named expected output file in tests/outputs/. This
# runner only supports expected passes; expected failures are not
# supported, but it would be easy to add by changing the output file
# naming convention to indicate whether the test was expected to succeed
# (and thus contain a valid parse output) or fail (and thus contain a
# daedalus error).
#
# Note that while it's ordinarily good practice to use set -e, it is
# deliberately skipped in this script because many intermediate steps
# need to be allowed to fail so we can continue running tests and
# collect the number of failures. Doing so with set -e is annoying to
# get right, so this script is carefully written to cope without it.

HERE=$(cd `dirname $0`; pwd)

# The repo root relative to this script's location.
ROOT=$HERE/../../

# The spec we want to use to parse the inputs.
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
    which daedalus || {
        ghc_ver=$(ghc --version | awk '{ print $NF }')
        find $ROOT/dist-newstyle -type f -name daedalus 2>/dev/null | grep $ghc_ver
    }
}

# parse <ENTRY> <INPUT_PATH> <EXPECTED_OUTPUT_PATH>; run Daedalus using
# the spec configured in $SPEC using the entry point ENTRY on the
# specified input file and compare the results to the contents of the
# specified expected output file.
function run_test_case {
    local entry=$1
    local input_file=$2
    local output_file=$3

    args="run $SPEC --entry=$entry -i $input_file"

    if [ -f "${output_file}" ]
    then
        tmpfile=$(mktemp)

        # Daedalus exits zero on success, in which case we want to diff
        # the output against the expected output. But if it fails (e.g.
        # due to a parse error) then we want to just emit that output
        # without diffing.
        if $DAEDALUS $args > ${tmpfile}
        then
            if diff --color $output_file $tmpfile
            then
                echo "  PASS      match: $output_file"
                return 0
            else
                echo "  FAIL      mismatch: $output_file"
                return 1
            fi
        else
            echo "  FAIL. Expected valid output, got:"
            cat $tmpfile
            return 1
        fi

        rm $tmpfile
    else
        echo "  Generated new expected output at ${output_file}"
        $DAEDALUS $args > ${output_file}
        return 0
    fi
}

cd $HERE
FILES=tests/inputs/*_request*.txt

DAEDALUS=$(find_daedalus)

if [ -z "$DAEDALUS" ]
then
    echo "Error: could not find daedalus in the PATH or in dist-newstyle/ in the repo."
    exit 1
fi

# Canonicalize the path and remove relative path segments
DAEDALUS=$(readlink -f $DAEDALUS)

echo "Using daedalus at:"
echo "  $DAEDALUS"
echo

num_failures=0

for request_file in $FILES
do
    filename=$(basename $request_file)
    output_file="tests/outputs/${filename}"

    echo ${request_file}:
    run_test_case HTTP_request $request_file $output_file
    num_failures=$((num_failures + $?))
    echo
done

if [ $num_failures -gt 0 ]
then
    echo "$num_failures failure(s)"
else
    echo "All tests passed."
fi

exit $num_failures
