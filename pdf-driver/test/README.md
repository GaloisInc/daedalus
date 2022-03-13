## TLDR, quickstart on how to run a test ##

      cd pdf-driver/test

      # Let's run the quick tests

      make remotecorpora      # rsyncs remote files to your machine
      make build              # not necessary, but let's do things piece by piece
      make quick              # builds run-testset tool and runs the quick tests
      make quick              # see how quick it is this time!

      # 'totext' is not included in the quick target, to run this 'test', do

      make test_totext_font-exercises/test-summary

      # it should say "0 unexpected variances", but this doesn't mean there
      # are no problems, we've already captured the problems in
      # "variances.filelist":

      $ cat test_totext_font-exercises/variances.filelist
      helloworld.pdf
        missing 'WORLD' - serious FIXME

      jagpdf_doc_font_encoding.pdf

      quickbrownetc.pdf
        simple issue: two 'f's versus the unicode ligature of "ff"
      $ ...

      # lines that start with space are comments, note that all three files
      # in the corpora `font-exercises` have an entry in `variances.filelist`

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
          expected/*.result-expected
          variances.filelist
    
  - and after running a test, the directory will have new files:

        test_TOOLNAME_CORPORANAME/
          ...
          results/*.{stdout,stderr,meta} -- raw results
          results/*.result-actual        -- final result, for comparison
                                         --  with expected/*.result-expected
          test-summary

Note that we don't test all the files in the `corpora/CORPORANAME` but only the
files for which we have an `expected/*.result-expected` file.  I.e.,
  
      for f in expected/*.result-expected; do
        base=basenameNoSuffix(f)
        run the test on this input-file:  corpora/CORPORANAME/$base
      done

These tools currently are supported, by name (see `src/run-testset/Tools.hs`):
  - validatePDF : `pdf-hs-driver ...`
  - totext      : `pdf-hs-driver -t ...`
  - cmap-sf     : parses CMAP (simple fonts)
  - cmap-cid    : parses CMAP (CID fonts)

## What we store in the repo ##

The following generated files will be stored in the repo, they are
(intentionally) small and should help us to keep track of functional 
changes:

      test_TOOLNAME_CORPORANAME/
        results/*.result-actual
        results/*.diff
        test-summary
   
## Running a Test ##

You run a test thus (or `cabal v2-run -- run-testset ...`)

    run-testset --tool=TOOLNAME --corpora=CORPORANAME
  
### Passing a Test & variances.filelist ###

For every file from the (designated) corpora we want tested, e.g.,

      corpora/CORP/myfile1.pdf

we need to indicate the expected value by having this file

      test_validatePDF_CORP/expected/myfile1.pdf.result-expected

We also have a notion of 'variances', captured in `variances.filelist` in which
we record the files for which these are not equivalent (tool specific):

      test_validatePDF_CORP/expected/myfile1.pdf.result-expected
      test_validatePDF_CORP/results/myfile1.pdf.result-actual

Any line in `variances.filelist` that starts with a space is a comment.

Passing a test ONLY means that the 'variances' exactly match the test results:
For a test to "pass" you want to see "0 problems" in the test-summary file.
There are two kinds of problems:

   1. Files where result `=/=` expected but no variance specified.
   2. Files where result `==` expected but a variance is specified.

## Expected and Actual

Depending on the tool the `run-testset` program may compared the expected 
value and the actual value differently.
- totext : strict equality of bytes, which in fact is not very useful!
- validatePDF : the expected value is a regular expression.

For `totext`, you can see the differences between the actual and 
the expected results by (e.g.)
```
cd test/test_totext_font-exercises
../../scripts/show-test-diff.sh helloworld.pdf
```

For `validatePDF` you can view the diff files, e.g.,
```
   test_validatePDF_2020-03-eval/results/*.diff
```
One could view all the false positives from `validatePDF` tool by
```
   grep false-positive test_validatePDF_*/results/*.diff
```

## Dealing with Timeouts 

We want to have *timeouts* but, for long running tests that may or may not
finish, this makes our results non-deterministic.
For `validatePDF` one can add "|Timeout" to the expected regex.
But this might cause difficulties with files in the variances.filelist 
when they timeout: the test *looks like* a success, thus we see "unexpected variances", e.g.,
```
2 unexpected variance(s):
 Files where result == expected but a variance is specified:
  0737_ae24537e723bae0cce6d51206855710b0184f4920e41bcfc2888e1f75f714080.pdf
  0989_fcc3d37aa21e4e1f7c076e835596fd18fcac90a8e14691e6ff47a40858a0e98c.pdf
```

Here's an idiom for re-testing files that have timed out:
```
cd pdf-driver/test
for f in $(grep -l Timeout test_validatePDF_2020-03-eval/results/*.result-actual | sed 's/\.result-actual//')
do 
 rm $f.*
done
RUNTESTSET_TIMEOUT_IN_SECS=600 make long-status
```

## Details & Implementation ##

Refer to `Makefile` for examples and some further details.

The `run-testset` program uses `shake` (a Haskell build system library) to 
drive test invocation and checking, achieving "Make" like efficiency, see
`src/RunTestSet.hs` for the code.

Although `run-testset` does a lot of make-like dependency checking, 
it does *not* have a dependency on `pdf-hs-driver`, i.e., 
it does not regenerate test outputs when `pdf-hs-driver` is newer.

So to regenerate all test outputs, one needs to first purge the 
test file outputs with one of
```
  make quick-clean
  make long-clean
``` 
This does remove files from the repo, so you should `make {quick,long}` 
after you do the `make {quick,long}-clean`.

## test specific documentation ##
### test_validatePDF_2020-03-eval ###

See src/CreateExpected.hs for how to create these files

      test_validatePDF_2020-03-eval/expected/*.result-expected

The PDFs under test are not in the repo, but are expected to be in this
directory: `corpora/2020-03-eval/`.  You can either
 - put a symlink there
 - `make corpora/2020-03-eval/` which will create the directory and copy the
   files from `nessy.dev.galois.com`

