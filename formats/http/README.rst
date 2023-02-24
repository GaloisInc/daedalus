HTTP Parser
===========

This directory contains a Daedalus specification for parsing HTTP 1.1
messages and a test suite for testing the parser on a corpus of inputs.
The parser specification includes reference links to various HTTP
specifications and ABNF grammar fragments that express the requirements
used to build the parser.

The HTTP 1.1 parser entry points are declared in ``HTTP-1.1.ddl``. They
are:

* ``HTTP_request`` - for parsing request messages
* ``HTTP_status`` - for parsing response messages

Running the tests
-----------------

The test suite is run by running ``run-tests.sh``. The test suite works
as follows:

* Files in ``tests/inputs/`` are parsed with Daedalus using the
  ``HTTP_request`` entry point. For each input file, the Daedalus output
  is compared to an expected output file in ``tests/outputs/`` by the
  same name as the input file. If a test's output differs from its
  expected output, it is considered a failure and the test suite runner
  script reports it accordingly. If a test results in a Daedalus error,
  the error is reported to the console.
* If a file in ``tests/inputs/`` has no corresponding file in
  ``tests/outputs/``, the test runner script will save the Daedalus
  output to that path. Future test suite runs will then compare the
  Daedalus output to the generated file.

Adding new test cases can be done as follows:

* Create a new test input file (HTTP request) in ``tests/inputs/``.
* Run ``run-tests.sh`` to save the output of the test in
  ``tests/outputs/``.
* Check that the output is as expected.
* Add the input and output files to Git with ``git add`` and commit.
