# Stack-based Polyglots

Stack polyglots concatenate two data formats.  A "base" format must admit a
prefix cavity at the beginning of the file, and a "top" format must admit a
suffix cavity at the end of the file.  The polyglot is formed by appending the
base content to the top content.

As an example, JPG data must appear at the beginning of a file, but it
tolerates extra data at the end of the file.  PDF, on the other hand, can start
anywhere in the file.  It forms the following stack-based polyglot.
```
|---- top ----- | --- base ---- |
 < JPG content>   <PDF content>
```

[stack-shape.ddl](stack-shape.ddl) is a Daedalus specification describing the
stack-based polyglot skeleton.

## Structure of this Directory

This directory contains Daedalus files in two forms:

1. `FMT1-FMT2.ddl` is a polyglot skeleton combining two specific formats.  It references `FMT1.ddl` and `FMT2.ddl`.
2. `FMT.ddl` defines characteristics of a format required for Talos to generate stack polyglots.  In this case, it could be an arbitrary base or top format specification, or a specific byte sequence representing a file to embed in the polyglot.

There are also a collection of scripts specific to stack-based polyglots:

* [gen-skeleton.sh](gen-skeleton.sh) creates `FMT1-FMT2.ddl` files, given FMT1 and FMT2.
* [get-files.sh](get-files.sh) creates `FMT.ddl` files.  Given a directory containing concrete examples of a collection of formats (e.g. `sample-1.jpg`, `sample-2.gif`, etc.), this script will produce one `FMT.ddl` for each format (e.g. `JPG.ddl`, `GIF.ddl`) that embeds the concrete example as a byte sequence.  This allows Talos to form polyglots from specific input files.
* [gen-phar-polyglot.sh](gen-phar-polyglot.sh) uses Talos to generate a PHAR polyglot combined with a given extension, e.g. JPG.  Given a format FMT, it assumes the PHAR-FMT.ddl skeleton exists, as well as PHAR.ddl and FMT.ddl.  The resulting file is a polyglot that can be executed using `php <output>.phar`.

These scripts rely on helpers in the [scripts](../scripts) directory.

## How to Generate a Polyglot

For PDF- and ZIP-based polyglots, simply run
```bash
talos -n 1 FMT1-FMT2.ddl > output.fmt
```

PHAR-based polyglots require a postprocessing step to attach a PHAR signature
to the file that Talos generates.  Use the
[gen-phar-polyglot.sh](gen-phar-polyglot.sh) script to generate a PHAR
polyglot.
```bash
# This script produces a file named phar-jpg.phar.jpg.
gen-phar-polyglot.sh JPG

# Run as a PHP archive.
php phar-jpg.phar.jpg

# Open as a JPG using your favorite viewer.
```
