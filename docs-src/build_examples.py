"""
Module: build_examples.py
Description: Write DaeDaLus tutorial examples to a `downloads` directory
Copyright: (c) Galois, Inc. 2022
License: N/A
Maintainer: cphifer@galois.com
Stability: experimental
Portability: N/A

This script walks the `examples` directory and packages up all DaeDaLus files
into a more-readable `downloads` directory, which is compressed and provided as
a download to the reader.

This code was borrowed from the Galois SAW Training repository and modified to
suit the needs of the DaeDaLus tutorial.
"""

import os
import os.path
from pathlib import Path
import re


file_pat = r'^[a-zA-Z0-9_-]+\.ddl$'

# Comments beginning with "-- BEGIN " and "-- END " will be stripped from the
# downloadable output
comment_pat = r'^\s*--\s+(BEGIN|END).+$'

# use_file(filename) returns True if and only if filename matches file_pat,
# in other words if filename should be included in the downloads
def use_file(filename):
    return True if re.match(file_pat, filename) else False

# generate_downloads(start, end) walks the directory tree beginning at
# start, and writes the files matching file_pat (with the comments matching
# comment_pat stripped) to a matching directory tree starting at end.
def generate_downloads(start, end):
    for this_dir, _, files in os.walk(start):
        for f in files:
            if use_file(f):
                in_file = os.path.join(this_dir, f)
                out_file = os.path.join(end, in_file)
                with open(in_file) as the_file:
                    out_contents = ''.join(line for line in the_file if not re.match(comment_pat, line))

                Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)

                with open(out_file, "w") as out:
                    out.write(out_contents)

if __name__ == "__main__":
    generate_downloads("source/examples", "build/downloads")
