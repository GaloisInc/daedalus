## Overview ##

Overall structure of the testing infrastructure
  
      src/
      corpora/             -- the files to test
        CORPORANAME1/*       -- pdf (or anything, no suffix required) files for testing
        CORPORANAME2/*       -- pdf (or anything, no suffix required) files for testing
        ...
      test_TOOLNAME_CORPORANAME1/
      test_TOOLNAME_CORPORANAME2/
      ...

## Corporas ##

Each test is run on a single corpora of files (in `corpora/NAME`), some of
these, due to size, are not part of the repo, to nab these corpora files do
this:

      make remotecorpora

Regarding specific corporas

  - pdf20examples : the (6) files are from Peter Wyatt's repo of good PDF-2.0 files:
    https://github.com/pdf-association/pdf20examples

  - govdocs-subset-a : 21 "random" files from govdocs

## Testset directories (`test_TOOLNAME_CORPORANAME/`) ##

Each testset directory (of form `test_TOOLNAME_CORPORANAME/`)
  - has an implicit "tool" `TOOLNAME` that is being run on the pdf (or ...) files.
  - has a given 'corpora' that is being tested (`corpora/CORPORANAME/`)
  - starts with this given structure:

        test_TOOLNAME_CORPORANAME/
          expctd/*.result-expctd
          variances.filelist
    
  - and after running a test, the directory will have new files:

        test_TOOLNAME_CORPORANAME/
          ...
          results/*.{stdout,stderr,meta} -- raw results
          results/*.result-actual        -- final result, for comparison
                                         --  with expctd/*.result-expctd
          test-summary

Note that we don't test all the files in the `corpora/CORPORANAME` but only the
files for which we have an `expctd/*.result-expctd` file.  I.e.,
  
      for f in expctd/*.result-expctd; do
        base=basenameNoSuffix(f)
        run the test on this input-file:  corpora/CORPORANAME/$base
      done

Two tools currently are supported, by name (see `src/RunTestSet.hs`):
  - validatePDF : `pdf-hs-driver ...`
  - totext      : `pdf-hs-driver ...`

## What to store in the repo ##

The following generated files will be stored in the repo, they are
(intentionally) small and should help us to keep track of functional 
changes:

      test_TOOLNAME_CORPORANAME/
        results/*.meta
        results/*.result-actual
        test-summary
   
The `*.meta` files will change a lot (as they have the runtime) but we still
want to keep track of this.
 
## Running a Test ##

You run a test thus (or `cabal v2-run -- run-testset ...`)

    run-testset TOOLNAME CORPORANAME
  
### Passing a Test & variances.filelist ###

For every file from the (designated) corpora we want tested, e.g.,

      corpora/CORP/myfile1.pdf

we need to indicate the expected value by having this file

      test_validatePDF_CORP/expctd/myfile1.pdf.result-expctd

We also have a notion of 'variances', captured in `variances.filelist` in which
we record the files for which these are not equivalent (tool specific):

      test_validatePDF_CORP/expctd/myfile1.pdf.result-expctd
      test_validatePDF_CORP/results/myfile1.pdf.result-actual

Any line in `variances.filelist` that starts with a space is a comment.

Passing a test ONLY means that the 'variances' exactly match the test results:
For a test to "pass" you want to see "0 problems" in the test-summary file.
There are two kinds of problems:

   1. Files where result `=/=` expctd but no variance specified.
   2. Files where result `==` expctd but a variance is specified.

## Details & Implementation ##

Refer to `Makefile` for examples and some further details.

The `run-testset` program uses `shake` (a Haskell build system library) to 
drive test invocation and checking, achieving "Make" like efficiency, see
`src/RunTestSet.hs` for the code.

## test specific documentation ##
### test_validatePDF_2020-03-eval ###

See src/CreateExpected.hs for how to create these files

      test_validatePDF_2020-03-eval/expctd/*.result-expctd

The PDFs under test are not in the repo, but are expected to be in this
directory: `corpora/2020-03-eval/`.  You can either
 - put a symlink there
 - `make corpora/2020-03-eval/` which will create the directory and copy the
   files from `nessy.dev.galois.com`

