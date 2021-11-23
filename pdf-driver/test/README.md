FIXME: needs work:

The `runtest` binary uses `shake` (a Haskell build system library) to 
drive test generation and checking with "Make" like efficiency.

Each test directory
  - has an implicit "tool" that is being run on the pdf files.
  - has a given 'corpora' that is being tested
  - starts with this given structure:
      ```
      test-toolname-corporaname/
        gold/*.result-gold
        variances.filelist
      ```
    
  - and after running a test, the directory will have new files:
      ```
      test-toolname-corporaname/
        ...
        results/*.{stdout,stderr,meta} -- raw results
        results/*.result-actual        -- final result, for comparison
                                       -- with gold/*.result-gold
        test-summary
      ```
    
We support two tools currently, by name,
  - driver : `pdf-hs-driver ...`
  - totext : `pdf-hs-driver ...`
      
You run a test thus

  `test TOOLNAME corp--CORPNAME/pdfs/`
  
Refer to Makefile for examples.
