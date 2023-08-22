#!/usr/bin/env bash

function print_help {
  echo "usage: $(basename $0) DIR"
}

if [ $# -ne 1 ]; then
  print_help
  exit 1
fi

FILE_DIR=$1
for f in $(ls $FILE_DIR/*); do
  name="$(basename $f)"
  base_ext="${name##*.}"
  ext=${base_ext^^}
  ../scripts/wrap-in-ddl ${ext} $f > ${ext}.ddl
done
