#!/bin/bash
# Run dex validation error tests.
# Usage: ./run-tests.sh [test-names...]
# If no names given, runs all .dex files in the current directory.
#
# These tests verify that dex correctly detects and reports validation errors.
# Each test is expected to FAIL validation with a specific error.
#
# For each test.dex file:
# - If test.dex.expected doesn't exist: run dex and save output to test.dex.expected
# - If test.dex.expected exists: run dex and compare output against the expected file

set -euo pipefail

# Get absolute path to script directory and project root
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TOP="$(cd "$SCRIPT_DIR/../../.." && pwd)"
DEX="$TOP/bin/dex"
DEX_LIB="$TOP/dex/lib"
DDL_LIB="$TOP/lib"

cd "$SCRIPT_DIR"

# Collect test files
tests=()
if [ $# -eq 0 ]; then
  for f in [0-9][0-9]-*.dex; do
    [ -f "$f" ] && tests+=("$f")
  done
else
  tests=("$@")
fi

# Sort tests by number
IFS=$'\n' tests=($(sort -V <<<"${tests[*]}"))
unset IFS

pass=0
fail=0
created=0
errors=""

for test_file in "${tests[@]}"; do
  printf "%-40s " "$test_file"

  expected_file="$test_file.expected"

  # Run dex on the test file, expecting it to fail
  # Need to specify paths for dex and ddl libraries
  # --dex-path points to dex/lib (for CPP.dex)
  # --ddl-path points to test directory (for spec.ddl)
  # --ddl-path points to daedalus/lib (for Daedalus.ddl)
  output=$("$DEX" "$test_file" \
    --dex-path="$DEX_LIB" \
    --ddl-path="$SCRIPT_DIR" \
    --ddl-path="$DDL_LIB" \
    2>&1) || rc=$?

  # Check that dex failed (non-zero exit code) - these are error tests!
  if [ ${rc:-0} -eq 0 ]; then
    echo "FAIL (expected validation error, but succeeded)"
    errors="$errors  $test_file: expected to fail but succeeded\n"
    fail=$((fail + 1))
    continue
  fi

  # If expected file doesn't exist, create it
  if [ ! -f "$expected_file" ]; then
    echo "$output" > "$expected_file"
    echo "CREATED $expected_file"
    created=$((created + 1))
    continue
  fi

  # Compare output with expected
  expected=$(cat "$expected_file")
  if [ "$output" = "$expected" ]; then
    echo "OK"
    pass=$((pass + 1))
  else
    echo "FAIL (output mismatch)"
    echo "  Diff (expected vs actual):"
    diff -u <(echo "$expected") <(echo "$output") | head -20 | sed 's/^/    /'
    errors="$errors  $test_file: output mismatch\n"
    fail=$((fail + 1))
  fi
done

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "$pass passed, $fail failed, $created created"

if [ $created -gt 0 ]; then
  echo ""
  echo "Created $created expected output files."
  echo "Please review them and run the tests again."
  exit 0
fi

if [ $fail -ne 0 ]; then
  printf "\nFailures:\n$errors"
  exit 1
fi

if [ $pass -eq 0 ]; then
  echo "FAIL: no tests passed"
  exit 1
fi

echo "All validation error tests passed!"
