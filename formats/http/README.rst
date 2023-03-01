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

The test suite is run by running ``run-tests.sh``. The test suite runner
runs on test cases from two directories:

* ``tests/http1/requests/`` for testing parsing of HTTP 1.1 request
  messages
* ``tests/http1/responses/`` for testing parsing of HTTP 1.1 response
  messages

Each test case directory has two subdirectories: ``inputs`` and
``outputs``. Each file in an ``inputs`` subdirectory is a test case that
is parsed by Daedalus; each file in ``outputs`` is a file with the same
name as a test case giving the expected Daedalus output when parsing the
corresponding input file.

The test suite works as follows:

* Files in an input directory (e.g. ``tests/http1/requests/inputs/``)
  are parsed with Daedalus using the associated entry point
  (e.g. ``HTTP_request``). For each input file, the Daedalus
  output is compared to an expected output file (e.g. in
  ``tests/http1/requests/outputs/``) by the same name as the input file.
  If a test's output differs from its expected output, it is considered
  a failure and the test suite runner script reports it accordingly.
  If a test results in a Daedalus error, the error is reported to the
  console.
* If an input file has no corresponding file, the test runner script
  will save the Daedalus output to the output file path. Future test
  suite runs will then compare the Daedalus output to the generated
  file.

Adding new test cases can be done as follows:

* Create a new test input file (e.g. an HTTP request in
  ``tests/http1/requests/inputs/``)
* Run ``run-tests.sh`` to save the output of the test (e.g. to a file in
  ``tests/http1/requests/outputs/``)
* Check that the output is as expected; this means checking on whether
  the generated output file contains either a successful parse tree OR a
  ``daedalus`` error message.
* Add the input and output files to Git with ``git add`` and commit.
