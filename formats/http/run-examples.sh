#!/usr/bin/env bash

# Run the Daedalus tool to parse example inputs using the HTTP
# spec. This attempts to use daedalus in the PATH; if it cannot be
# found there, a build of daedalus is triggered at the root of this
# repository and that build is used to parse the examples.
#
# This script runs the parser against all of the request example files
# in the examples/ subdirectory, i.e., those named *_request.txt.

set -e

HERE=$(cd `dirname $0`; pwd)
SPEC=$HERE/HTTP.ddl

# in_path <PROG>; exit code is zero if the specified program is found in
# the PATH. Produces no output.
function in_path {
    local cmd=$1
    which $cmd 2>&1 >/dev/null
}

# parse <ENTRY> <PATH>; run Daedalus using the spec configured in $SPEC
# using the entry point ENTRY on the specified input file. Uses daedalus
# from the PATH if possible; otherwise it builds daedalus at the root of
# the repo using cabal.
function parse {
    local entry=$1
    local input_file=$2

    args="run $SPEC --entry=$entry -i $input_file"
    if in_path daedalus
    then
        daedalus $args
    else
        cabal new-run daedalus -- $args
    fi
}

cd $HERE/../..

FILES="$*"

if [ -z "$FILES" ]
then
    FILES=$HERE/examples/*_request*.txt
fi

for request_file in $FILES
do
    echo ${request_file}:
    parse HTTP_request $request_file
done
