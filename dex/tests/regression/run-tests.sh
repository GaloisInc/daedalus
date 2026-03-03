#!/bin/bash
# Run dex regression tests.
# Usage: ./run-tests.sh [test-names...]
# If no names given, runs all tests found as subdirectories with expected.txt.

set -euo pipefail

cd "$(dirname "$0")"

# Platform detection for ASAN leak detection
# Disable leak detection on ARM Macs (not currently supported)
if [[ "$(uname -s)" == "Darwin" ]] && [[ "$(uname -m)" == "arm64" ]]; then
  export ASAN_OPTIONS=detect_leaks=0
else
  export ASAN_OPTIONS=detect_leaks=1
fi

tests=("$@")
if [ ${#tests[@]} -eq 0 ]; then
  for d in */; do
    t="${d%/}"
    [ -f "$t/expected.txt" ] && tests+=("$t")
  done
fi

pass=0
fail=0
build_fail=0
errors=""

for t in "${tests[@]}"; do
  printf "%-25s " "$t"
  driver="build/$t/driver"

  # Build the test if needed
  if ! make "$t" > /dev/null 2>&1; then
    echo "FAIL (build error)"
    errors="$errors  $t: build failed\n"
    build_fail=$((build_fail + 1))
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
echo "$pass passed, $fail failed, $build_fail build failures"
if [ $pass -eq 0 ]; then
  echo "FAIL: no tests passed"
  exit 1
fi
if [ $build_fail -ne 0 ]; then
  echo "FAIL: $build_fail test(s) failed to build"
fi
if [ $fail -ne 0 ] || [ $build_fail -ne 0 ]; then
  printf "\nFailures:\n$errors"
  exit 1
fi
