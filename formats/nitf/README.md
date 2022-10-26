# Running DaeDaLus NITF parser #

To run the NITF parser on NITF file `file.ntf` from directory
`tests/nitf`, run the following command:

```
> cabal run ../:daedalus -- nitf_main.ddl -i file.ntf
```

# Parsing the GWG.NGA test files #

To test the DaeDaLus parser on the GWG.NWA sample files,

1. Create a directory in the repos named `nitf/gwg.nga.mil_samples`
   that directly contains the test files;
2. Run the following command:
```
> ./test_milsamples.sh
```

# Parsing the JITC test files #

To test the DaeDaLus parser on the JITC sample files,

1. Install the test files to a directory named `nitf/JITC\ -\
   NITF\ Test\ Data\ -\ Set\ 1`. This directory should itself contain
   a subdirectories named `POS` and `NEG` that each contain NITF files
   that the parser is expected to accept and reject, respectively.
2. Run the following command:
```
> ./test_jitc.sh
```

# Generating the C++ NITF parser #

```
cabal run daedalus -- compile-c++ nitf/nitf_main.ddl --out-dir=nitf_cpp_parser
cd nitf_cpp_parser
make parser
```

# Comparison with other NITF parsing tools #

The following two sections are notes for how to run the NITRO tools or GDAL tools.
They require to be adapted to the local machine.

## Testing NITRO parser ##

The following instructions are just notes and would likely require to be adapted.

Get NITRO at
https://github.com/mdaus/nitro

```bash
git clone https://github.com/mdaus/nitro
```

Start a docker container from a base Ubuntu image and run the following commands, making sure the NITRO repo is volume mounted

```bash
apt-get update
apt-get install cmake -y
apt-get install g++
apt-get install python3
apt-get install python3-pip
pip install numpy
apt-get install libgmp-dev # required to build daedalus c++ parsers

<FROM nitro repo>
mkdir build
cd build
cmake ..
cmake --build . -j
cmake --build . --target install
ctest

<save the docker image using docker save>

docker run -it \
  -v <PATH>/NITF/NITRO/nitro:/nitro \
  -v /tmp/JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1:/tmp/JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1:ro \
  -v /tmp/gwg.nga.mil_samples:/tmp/gwg.nga.mil_samples:ro \
  -v <PATH>/DAEDALUS/daedalus/nitf/:/daedalus/nitf \
  -v <PATH>/DAEDALUS/daedalus/nitf_cpp_parser/:/daedalus/nitf_cpp_parser \
  nitro:v2 \
  /bin/bash

# modify regression.sh so that it runs the NITRO  parser
```

## Testing GDAL parser ##

The GDAL repo is available at the following address but it not necessary
https://github.com/OSGeo/gdal

```
docker pull osgeo/gdal:latest
docker run -it \
  -v /tmp/gwg.nga.mil_samples:/tmp/gwg.nga.mil_samples:ro \
  -v /tmp/JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1:/tmp/JITC\ -\ NITF\ Test\ Data\ -\ Set\ 1:ro \
  -v <PATH>/DAEDALUS/daedalus/nitf:/daedalus/nitf \
  osgeo/gdal:latest \
  /bin/bash

# modify regression.sh so that it runs the GDAL  parser
```

