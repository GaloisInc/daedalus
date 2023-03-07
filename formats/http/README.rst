HTTP Parsers
============

This directory contains Daedalus specifications for parsing HTTP 1.1
messages and HTTP 2 frames and a test suite for testing the parsers on
a corpus of inputs for each version of HTTP. The parser specifications
include reference links to various HTTP specifications and ABNF grammar
fragments that express the requirements used to build the parsers.

The HTTP 1.1 parser entry points are declared in ``HTTP-1.1.ddl``. They
are:

* ``HTTP_request`` - for parsing request messages
* ``HTTP_status`` - for parsing response messages

The HTTP 2 parser's entry point, ``HTTP2_frame``, is declared in
``HTTP-2.ddl``.

Running the tests
-----------------

The test suite is run by running ``run-tests.sh``. The test suite runner
runs on test cases from three directories:

* ``tests/http1/requests/`` for testing parsing of HTTP 1.1 request
  messages
* ``tests/http1/responses/`` for testing parsing of text HTTP 1.1 response
  messages
* ``tests/http2/frames/`` for testing parsing of HTTP 2 frames

Each test case directory has two subdirectories: ``inputs`` and
``outputs``. Each file in an ``inputs`` subdirectory is a test case
input file. Each file in ``outputs``, which should be tracked in
revision control, is a file with the same name as a test case giving the
expected Daedalus output when parsing the corresponding input file.

The test suite works as follows:

* Files in an input directory (e.g. ``tests/http1/requests/inputs/``)
  are parsed with Daedalus using the associated entry point (e.g.
  ``HTTP_request``).

  * For HTTP 1.1, the input file must be a file with ``.txt`` extension
    and must contain a single HTTP 1.1 request or response (depending on
    the input directory).
  * For HTTP 2, the input file must be a file with ``.xxd`` extension
    and must contain an ``xxd``-encoded HTTP 2 frame. This allows easer
    user editing of test cases than editing binary files directly, and
    ``.xxd`` files can be equipped with comments to help explain what
    the test case is expected to contain. The test suite runner will
    automatically generate a binary file from the ``.xxd`` encoding and
    pass the binary file to Daedalus for parsing.

  For each input file, the Daedalus output is compared to an expected
  output file (e.g. in ``tests/http1/requests/outputs/``) by the same
  name as the input file. If a test's output differs from its expected
  output, it is considered a failure and the test suite runner script
  reports it accordingly. If a test results in a Daedalus error, the
  error is reported to the console.
* If an input file has no corresponding file, the test runner script
  will save the Daedalus output to the output file path. Future test
  suite runs will then compare the Daedalus output to the generated
  file.

Adding new test cases can be done as follows:

* Create a new test input file (e.g. a text HTTP 1.1 request in
  ``tests/http1/requests/inputs/new_request.txt`` or a binary HTTP 2
  frame in ``tests/http2/frames/inputs/new_frame.xxd``)
* Run ``run-tests.sh`` to save the output of the test (e.g. to a file in
  ``tests/http1/requests/outputs/``)
* Check that the output is as expected; this means checking on whether
  the generated output file contains either a successful parse tree OR a
  ``daedalus`` error message.
* Add the input and output files to Git with ``git add`` and commit.

Status and Next Steps
=====================

HTTP 1.1 Parser
---------------

The HTTP 1.1 parser provided in this directory supports HTTP 1.1
requests and responses and parses message bodies with and without the
``chunked`` transfer encoding.

HTTP 2 Parser
-------------

The HTTP 2 parser provided in this directory parses all binary HTTP
frames specified in the HTTP 2 specification. Some next steps for future
work are:

* Support multi-frame parsing by parsing server and client connection
  prefaces as described in Section 3.4 of the HTTP 2 specification. At
  the time of this writing, the parser only parses individual frames
  and does not parse any other expected connection traffic such as the
  connection preface octet sequences.

* Support concatenation of field blocks spanning multiple HTTP 2 frames.
  At the time of this writing, the parser parses frames that include
  field block fragments but provides the fragments as octet sequences
  and leaves concatenation up to the calling application.

* Support decompression of HPACK-compressed field blocks. At the time
  of this writing, the parser provides no support for decompressing
  HPACK-compressed field blocks.

* Support protocol extensions. At the time of this writing, the parser
  is not designed specifically to permit extensibility along the lines
  of what is specified in section 5.5 of the HTTP 2 specification.
