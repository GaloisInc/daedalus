
Talos and HTTP
==============

This directory contains a Dockerfile (and supporting scripts and DDL) to generate HTTP files via Talos.

Usage
-----

The script run_talos.sh takes as an argument the number of documents to generate.  It will take a number of minutes to run and does not produce any output to stdout.  All the output files are placed in /http-out in the container, which should be mounted as a local directory.

The following should work
```
$ mkdir out
$ docker build -t http-talos https://raw.githubusercontent.com/GaloisInc/daedalus/wip/talos-http-docker/http-talos/Dockerfile
...
$ docker run --rm -it -v $(pwd)/out/:/http/out http-talos ./run_talos.sh 10 
$ ls out
```
