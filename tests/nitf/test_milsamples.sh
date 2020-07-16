#!/bin/bash 

# This approach taken from http://mywiki.wooledge.org/BashFAQ/001

INPUT_DIR=./gwg.nga.mil_samples

find "$INPUT_DIR" -type f -iname '*.ntf' -o -iname '*.nitf' |  
while read -r file; do 
    printf '%s ... ' "$file"
    if cabal run ../../:daedalus -- nitf_main.ddl -i "$file" > /dev/null; then 
        printf "pass\n" 
    else 
        printf "fail\n"
    fi 
done 
