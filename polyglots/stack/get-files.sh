#!/usr/bin/env bash

function print_help {
  echo "usage: $(basename $0) DIR"
  echo ""
  echo "For each file x.ext in DIR, extract its bytes and embed them into EXT.ddl.  If more than one file with the same extension exist, the last (lexicographically) is extracted."
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
