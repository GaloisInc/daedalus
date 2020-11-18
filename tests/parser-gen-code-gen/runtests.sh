#!/usr/bin/env bash

#NOTE: We are making an attempt in this script to stick to "older" bash,
#specifically trying to avoid a dependency on bash 4, so that it works
#on Macs and older Centos. This does make scripts a bit more verbose since
#useful commands are often missing.

#--------------------------------------------------------------------------
# Variables
#--------------------------------------------------------------------------

#The directory that contains the current script
#NOTE: This invocation should work in most cases, but is not perfect :|
#`dirname` + `readlink` will give a better answer; but the latter is not
#a bash/posix built-in. All-in-all fix this if it is unable to find the
#directory with the Makefile in it.
SCRIPTROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

#The git root directory
GITROOT=$(git rev-parse --show-toplevel)

#And the location of the runtime files
RTSDIR="${GITROOT}/rts-pgen-c"

#LOGLEVEL for running the tests. If there is an environment variable, we use that
#otherwise define our default
if [[ -z ${LOGLEVEL+x} ]]; then
    LOGLEVEL=NONE
fi

#--------------------------------------------------------------------------
# Functions
#--------------------------------------------------------------------------

#Get absolute path given a root directory and a file
function get_abs_path()
{
    pushd "$1" > /dev/null
    echo "$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
    popd > /dev/null
}

#Clean out the test directory
function clean_test()
{
    local abs_out_dir="$1"
    rm -rf "$abs_out_dir"
    mkdir -p "$abs_out_dir"
}

function copy_logs()
{
    local test_file="$1"
    local source_dir="$2"
    local dest_dir="$3"

    find "$dest_dir" -name '$test_file.*' -exec rm {} \;

    find "$source_dir" -name '*.log' -exec cp {} "$dest_dir" \;
    find "$source_dir" -name '*.stdout' -exec cp {} "$dest_dir" \;
    find "$source_dir" -name '*.stderr' -exec cp {} "$dest_dir" \;
}

#This function runs a single test and produces a result. This expects the full
#path to the test file as input.
#NOTE: This function freely changes directories to accomplish what it is doing
#So you cannot assume the final directory when this function is done
function run_test()
{
    #Read the test file
    local test_file="$1"

    echo -n "Starting Test $(basename "$test_file") .. "

    local IFS=
    exec 6<&0
    exec < "$test_file"
    read -r ddl_file
    read -r input_file
    exec 0<&6 6<&-

    #Resolve paths
    local base_dir="$(dirname "$test_file")"

    local abs_ddl_file=$(get_abs_path $base_dir $ddl_file)
    local abs_input_file=$(get_abs_path $base_dir $input_file)

    local results_dir="$SCRIPTROOT/test-results"
    mkdir -p "$results_dir"

    local abs_out_dir="/tmp/pgen-c-test"

    local golden_result_file="$test_file.stdout"

    #Validate the input files
    if [[ ! -f "$abs_ddl_file" ]]; then
        echo "FAILED (Unable to find Daedalus file: $abs_ddl_file)"
        return 2
    fi

    if [[ ! -f "$abs_input_file" ]]; then
        echo "FAILED (Unable to find input file: $abs_input_file)"
        return 2
    fi

    #Clean the out dir
    clean_test "$abs_out_dir"

    #Execute the generate command
    cd "$GITROOT"
    local daedalus_log_file="$abs_out_dir/$(basename "$test_file").daedalus.log"
    cabal v2-run exe:daedalus -- \
        --gen --compile-c++ --out-dir="$abs_out_dir" "$abs_ddl_file" \
        >"$daedalus_log_file" 2>&1

    if [[ $? -ne 0 ]]; then
        echo "Compiling $test_file failed"
        copy_logs "$test_file" "$abs_out_dir" "$results_dir"
        return 1
    fi

    #Build the generated grammar
    local make_log_file="$abs_out_dir/$(basename "$test_file").make.log"
    cp "$SCRIPTROOT/Makefile" "$abs_out_dir/Makefile"
    cd "$abs_out_dir"
    make clean > "$make_log_file" 2>&1
    make RTS=$RTSDIR LOGLEVEL=$LOGLEVEL >> "$make_log_file" 2>&1

    if [[ $? -ne 0 ]]; then
        echo "Compiling $test_file to C failed"
        copy_logs  "$test_file" "$abs_out_dir" "$results_dir"
        return 1
    fi

    #Run the generated executable
    local stdout_file="$abs_out_dir/$(basename "$test_file").stdout"
    local stderr_file="$abs_out_dir/$(basename "$test_file").stderr"
    # echo "./grammar $abs_input_file > $stdout_file 2> $stderr_file"
    cd "$abs_out_dir"
    ./grammar "$abs_input_file" > "$stdout_file" 2> "$stderr_file"

    #Check the results
    local diff_file="$abs_out_dir/$(basename "$test_file").diff"
    diff "$stdout_file" "$golden_result_file" > "$diff_file"
    if [[ $? -ne 0 ]]; then
        echo "FAILED"
    else
        echo "SUCCEEDED"
    fi

    #Copy all relevant files
    copy_logs  "$test_file" "$abs_out_dir" "$results_dir"
}

function run_all_tests()
{
    local test_files=$SCRIPTROOT/*.test
    for f in $test_files
    do
        run_test $f
    done
}

#--------------------------------------------------------------------------
# Main
#--------------------------------------------------------------------------

if [[ $# -ge 1 ]]; then
    current_dir="$(pwd)"
    for f in "$@"
    do
        f="$(get_abs_path "$current_dir" "$f")"
        run_test "$f"
    done
else
    run_all_tests
fi