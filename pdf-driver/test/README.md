FIXME: needs work:

The `runtest` binary uses `shake` (a Haskell build system library) to 
drive test generation and checking with "Make" like efficiency.

Common structure:
  ```
  src/
  corpora/            -- the files to test
    CORPNAME1/*       -- data
    CORPNAME1-get.sh  -- if needed, script to populate above directory
    ...
  ```

Each test directory
  - has an implicit "tool" that is being run on the pdf files.
  - has a given 'corpora' that is being tested
  - starts with this given structure:
      ```
      test--toolname--corporaname/
        expctd/*.result-expctd
        variances.filelist
      ```
    
  - and after running a test, the directory will have new files:
      ```
      test-toolname-corporaname/
        ...
        results/*.{stdout,stderr,meta} -- raw results
        results/*.result-actual        -- final result, for comparison
                                       --  with expctd/*.result-expctd
        test-summary
      ```
    
We support two tools currently, by name,
  - validatePDF : `pdf-hs-driver ...`
  - totext : `pdf-hs-driver ...`
      
You run a test thus

  `runtest TOOLNAME CORPNAME`
  
Refer to Makefile for examples.
