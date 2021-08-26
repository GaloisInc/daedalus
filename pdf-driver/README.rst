TEST BUILD FIRST
----------------

You want to make sure these build (execute from parent directory):

.. code-block:: bash
  
    cabal v2-build exe:pdf-hs-driver
    cabal v2-build exe:pdf-dom

    
TESTING
-------

.. code-block:: bash

    # in this directory:
    cd pdf-driver

    # nab test PDFs
    
    ./scripts/get-pdf-tests
    
    # or, since I already have a directory with the 2020-03-eval files, I do
      (cd pdf_tests; ln -s ~/r/attic/2020-03-eval .)

    # run pdf-hs-driver on all the above PDF files
    
    ./scripts/run-tests
    
    # The output of the going into    
    #   pdf_tests_actual/pdf_tests/2020-03-eval/
    # and the expected output is here
    #   pdf_tests_expected/pdf_tests/2020-03-eval/
    
    # You can get more detailed diffs (without re-running pdf-hs-driver) thusly:
    
    ./scripts/run-tests --diffonly

GUIDELINES
----------

The above tests are great when we're making non-functional changes to the
parser.  But if we make a functional change we now have 1000*2 file outputs

    pdf_tests_expected/pdf_tests/2020-03-eval/*.pdf.{stdout,stderr}
    
that may need need to change!  So, please remember that keeping the expected outputs up to
date is the only thing that makes these useful.

To update the expected outputs (from a fully vetted set of outputs :-):

.. code-block:: bash
 
    cp pdf_tests_actual/pdf_tests/2020-03-eval/* \
       pdf_tests_expected/pdf_tests/2020-03-eval/

EXAMPLE: Create Alternative Baselines
-------------------------------------

.. code-block:: bash

    cd pdf-driver
    mkdir test_workspace && cd test_workspace
    ln -s ../pdf_tests .
    cp -R ../pdf_tests_actual pdf_tests_expected
      # or from whatever directory (holding pdf_tests/2020-03-eval/) that you want to use.
    ../scripts/run-tests
    ../scripts/run-tests --diffonly


TODO
----

- create a smaller, more broadly covering, set of files as a regression test set.    
  - 1000 rather random files is not a useful regression test set; when our
    parser output changes, that's a lot of inputs to vet!
    
    
- might we add pdf_tests_actual/ to the repo so we can keep track of changes?
