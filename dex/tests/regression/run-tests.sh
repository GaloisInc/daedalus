#!/bin/bash
# Run dex regression tests.
# Usage: ./run-tests.sh [test-names...]
# If no names given, runs all tests found as subdirectories with expected.txt.

set -euo pipefail

cd "$(dirname "$0")"

tests=("$@")
if [ ${#tests[@]} -eq 0 ]; then
  for d in */; do
    t="${d%/}"
    [ -f "$t/expected.txt" ] && tests+=("$t")
  done
fi

pass=0
fail=0
skip=0
errors=""

for t in "${tests[@]}"; do
  printf "%-25s " "$t"
  driver="build/$t/driver"
  if [ ! -x "$driver" ]; then
    echo "SKIP (not built)"
    skip=$((skip + 1))
    continue
  fi
  actual=$("$driver" 2>&1) || {
    rc=$?
    echo "FAIL (exit code $rc)"
    if [ -n "$actual" ]; then
      echo "$actual" | head -20
    fi
    errors="$errors  $t: non-zero exit ($rc)\n"
    fail=$((fail + 1))
    continue
  }
  expected=$(cat "$t/expected.txt")
  if [ "$actual" = "$expected" ]; then
    echo "OK"
    pass=$((pass + 1))
  else
    echo "FAIL"
    diff <(echo "$expected") <(echo "$actual") | head -20
    errors="$errors  $t: output mismatch\n"
    fail=$((fail + 1))
  fi
done

echo ""
echo "$pass passed, $fail failed, $skip skipped"
if [ $pass -eq 0 ]; then
  echo "FAIL: no tests passed"
  exit 1
fi
if [ $skip -ne 0 ]; then
  echo "FAIL: $skip test(s) skipped (not built)"
  exit 1
fi
if [ $fail -ne 0 ]; then
  printf "\nFailures:\n$errors"
  exit 1
fi
