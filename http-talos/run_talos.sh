#!/bin/bash

OUT=/http-out/

if [ $# -lt 1 ];
then
    echo "Usage: run_talos.sh COUNT"
    exit 1
fi

COUNT=$1

mkdir -p out
rm -f out/http.bin.*

/usr/local/bin/talos -n $COUNT -O out/http.bin -t 'pathsymb max-depth=10 num-models=300 num-loop-elements=5' -a fl -e HTTP_request http/HTTP-1.1.ddl > /dev/null


