#!/bin/bash

# this script is idempotent

MY_PATH=$(dirname "$0")
DEST_DIR=$MY_PATH/../test/corpora

# directories of corpora we can nab with get-pdf-corpora.sh:
REMOTECORPORA="2020-03-eval govdocs-subset-a pdf20examples"
# REMOTECORPORA=govdocs-subset-a

get_abs_filename() {
  # $1 : relative filename
  echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
}

if [ $# == 1 ]; then
  SRC_DIR=$1
else
  echo "Usage: populate-and-link-indirect-corpora.sh SRCDIR"
  exit 1
fi

for rc in $REMOTECORPORA ; do
  DEST_DIR_CORPUS=$DEST_DIR/$rc
  echo "populating $SRC_DIR/$rc :"  
  $MY_PATH/get-pdf-corpora.sh -D $SRC_DIR $rc
    # no-op if $rc exists in $SRC_DIR

  echo; echo "linking to $SRC_DIR/$rc ...:"
  if test -e $DEST_DIR_CORPUS -o -L $DEST_DIR_CORPUS; then
    echo " $DEST_DIR_CORPUS already exists"
  else
    echo " linking to $rc from $DEST_DIR"
    ln -s $(get_abs_filename $SRC_DIR/$rc) $DEST_DIR/$rc
  fi
done
            
  
