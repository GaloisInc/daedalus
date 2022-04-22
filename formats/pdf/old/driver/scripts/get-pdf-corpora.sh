#!/bin/bash

MY_PATH=$(dirname "$0")

if [ $# -ge 2 -a "$1" = "-D" ]; then
  CORP_DIR=$2
  shift 2 
else
  CORP_DIR=$MY_PATH/../test/corpora
fi


HOST="nessy.dev.galois.com"
HOST_PATH="/media/data/raw"

if [ $# == 0 ]; then
  echo "Usage: get-pdf-corpora.sh [-D destdir] corporaname+"
  echo "Some potential corpora:"
  for i in $(ssh "$HOST" "ls -d $HOST_PATH/*/"); do basename $i; done
  exit 1
fi

for CORPUS in $*; do
  if test -e $CORP_DIR/$CORPUS -o -L $CORP_DIR/$CORPUS; then
    echo "$CORP_DIR/$CORPUS exists"
  else     
    echo "Downloading $CORPUS"
    rsync -avz "$HOST:$HOST_PATH/$CORPUS" $CORP_DIR/
  fi
done
