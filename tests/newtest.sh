#!/bin/bash

last=$(ls T*.ddl | sed 's/T\(.*\)\.ddl/\1/' | sort -n | tail -1)
# the 10# says to treat the number as decimal (not octal, due to the leading 0)
next=$(printf "T%.3d.ddl" $((10#$last + 1)))
touch $next
echo $next
