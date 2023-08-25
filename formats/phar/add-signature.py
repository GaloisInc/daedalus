#!/usr/bin/env python

import argparse
import hashlib
import shutil
import sys

def generate_signature(input_path):
    with open(input_path, "rb") as f:
        digest = hashlib.file_digest(f, "sha256")
        signature = digest.digest() + b'\x03\x00\x00\x00\x47\x42\x4d\x42'
        return signature

def main():
    parser = argparse.ArgumentParser(description="Generate and append a Phar signature.")
    parser.add_argument(dest="file", metavar="FILE", nargs=1)
    parser.add_argument('-o', dest="outfile", action="store", help="output file")
    args = parser.parse_args()

    signature = generate_signature(args.file[0])

    if args.outfile:
        if args.outfile != args.file[0]:
            shutil.copyfile(args.file[0], args.outfile)
        with open(args.outfile, "ab") as f:
            f.write(signature)
    else:
        with open(args.file[0], "rb") as f:
            contents = f.read()
            sys.stdout.buffer.write(contents)
            sys.stdout.buffer.write(signature)

if __name__ == '__main__':
    main()
