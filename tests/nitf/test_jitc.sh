#!/bin/bash 

# This approach taken from http://mywiki.wooledge.org/BashFAQ/001

printf "Positive tests (should pass):\n"
ls -d JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1/*/POS | 
while read -r dir; do 
    find "$dir" -type f -iname '*.ntf' -o -iname '*.nitf' | 
    while read -r file; do 
        printf ' %s ... ' "$file"
        if cabal run ../../:daedalus -- nitf_main.ddl -i "$file" > /dev/null; then 
            printf "pass\n" 
        else 
            printf "fail\n"
        fi 
    done 
done 

printf "\nNegative tests (should fail):\n"
ls -d JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1/*/NEG | 
while read -r dir; do 
    find "$dir" -type f -iname '*.ntf' -o -iname '*.nitf' | 
    while read -r file; do 
        printf ' %s ... ' "$file"
        if cabal run ../../:daedalus -- nitf_main.ddl -i "$file" > /dev/null; then 
            printf "pass\n" 
        else 
            printf "fail\n"
        fi 
    done 
done 