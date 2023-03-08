
Talos and NITF
==============

This directory contains a Dockerfile (and supporting scripts and DDL) to generate NITF files via Talos.

Usage
-----

The script run_talos.sh takes as an argument the number of documents to generate.  It will take a number of minutes to run and does not produce any output to stdout.  All the output files are placed in /nitf-out in the container, which should be mounted as a local directory.

The following should work
```
$ mkdir out
$ docker build -t nitf-talos https://raw.githubusercontent.com/GaloisInc/daedalus/wip/talos-nitf-docker/nitf-talos/Dockerfile
...
$ docker run --rm -it -v $(pwd)/out/:/nitf-out nitf-talos run_talos.sh 10 
$ ls out
```
