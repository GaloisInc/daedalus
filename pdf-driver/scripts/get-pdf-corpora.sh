#!/bin/bash

MY_PATH=$(dirname "$0")

VM="nessy.dev.galois.com"
VM_PATH="/media/data/raw"

if  [ $# == 0 ]; then
  echo "Need a corpus.  Some of these may work:"
  for i in $(ssh "$VM" "ls -d $VM_PATH/*/"); do basename $i; done
  exit 1
fi

CORP_DIR=$MY_PATH/../test/corpora

for CORPUS in $*; do
  if test -e $CORP_DIR/$CORPUS -o -L $CORP_DIR/$CORPUS; then
    echo "$CORP_DIR/$CORPUS exists"
  else     
    echo "Downloading $CORPUS"
    rsync -avz "$VM:$VM_PATH/$CORPUS" $CORP_DIR/
  fi
done

