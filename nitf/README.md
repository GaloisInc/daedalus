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
