== Overview ==

Common structure:
  ```
  src/
  corpora/             -- the files to test
    CORPORANAME1/*       -- pdf (or etc) files for testing
    CORPORANAME1-get.sh  -- if needed, script to populate above directory
    ...
  ```

Each test directory (of form `test_TOOLNAME_CORPORANAME/`)
  - has an implicit "tool" `TOOLNAME` that is being run on the pdf (or ...) files.
  - has a given 'corpora' that is being tested (`corpora/CORPORANAME/`)
  - starts with this given structure:
      ```
      test_TOOLNAME_CORPORANAME/
        expctd/*.result-expctd
        variances.filelist
      ```
    
  - and after running a test, the directory will have new files:
      ```
      test_TOOLNAME_CORPORANAME/
        ...
        results/*.{stdout,stderr,meta} -- raw results
        results/*.result-actual        -- final result, for comparison
                                       --  with expctd/*.result-expctd
        test-summary
      ```
    
Two tools currently are supported, by name (see src/Shakefile.hs):
  - validatePDF : `pdf-hs-driver ...`
  - totext      : `pdf-hs-driver ...`

== The repo ==

The following generated files will be stored in the repo, they are
(intentionally) small and should help us to keep track of functional 
changes:

    test_TOOLNAME_CORPORANAME/
      results/*.meta
      results/*.result-actual
      test-summary
   
 
== Run a Test ==

You run a test thus (or `cabal v2-run -- run-testset ...`)

  `run-testset TOOLNAME CORPORANAME`
  
== Examples ==

Refer to Makefile for examples.

== Implementation ==

The `runtest` binary uses `shake` (a Haskell build system library) to 
drive test invocation and checking, achieving "Make" like efficiency.

== test specific documentation ==
=== test_validatePDF_2020-03-eval ===

See src/CreateExpected.hs for how to create these files

    test_validatePDF_2020-03-eval/expctd/*.result-expctd

The required pdfs must be in corpora/2020-03-eval/*, which are not part
of the repo.

