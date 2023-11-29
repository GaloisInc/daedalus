#!/usr/bin/env bash

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

function print_help {
  echo "usage: $(basename $0) <extension>"
}

if [ $# -ne 1 ]; then
  print_help
  exit 1
fi

EXT=${1^^}
EXT_LOWER=$(echo $EXT | tr '[:upper:]' '[:lower:]')

INPUT=PHAR-$EXT.ddl
OUTPUT=phar-$EXT_LOWER.phar.$EXT_LOWER

# Generate the Phar polyglot (without the Phar signature).
talos -n 1 $INPUT > $OUTPUT.contents

# Add the Phar signature.
$SCRIPT_DIR/../../formats/phar/add-signature.py $OUTPUT.contents -o $OUTPUT

# Clean up.
rm $OUTPUT.contents
