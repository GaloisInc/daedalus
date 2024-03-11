Getting Started
===============

We provide a Docker image which contains a development environment including:
  * `daealus`: the parser generator described in our paper
  * `daedalus-language-server`:
    a LSP server, useful when working with Daedalus specifications in VS Code,
    together with its corresponding VS Code client.
  * General development tools for working with Haskell and C++ code.
  * Other parser generators we use for benchamrking, namely:
    - antlr
    - kaitai-struct-compiler

To use the image you'll need [docker][1].
Optionally, you may also use VS Code[2],
with the [remote container extension][3].

If you are not planning to use VS Code you may start docker using:

    > docker run -it daedalus:08-March-2024 /bin/bash

This should start the image, attach to the running container, and
start a shell on the terminal.

If you'd like to use VS Code, then you don't need to attach to it on the
command line:

    > docker run daedalus:08-March-2024

Instead, from within VS Code, you can issue the command
"Dev Containers: Attach to Running Container" [4].  You can then use
the VS Code terminal to interacti with `daedalus`,
but you can also use the editor, which should give you Daedalus code
syntax highlighting, point out errors in place, or show you the
type of various expressions in a Daedalus specification.

Once you are in the container image, you should see a directory
called `daedalus`: this is the source code repository for our tool,
which is also available on [github][5].


Example Daedalus Specifications
===============================

There are some example specifications, in various degress of completeness,
in directory:

    > ls daedalus/formats

Some interesting ones to have a look at, in increasing complexity are:

    * utf8/utf8.ddl
    * json/JSON.ddl
    * png/PNG.ddl
    * midi.ddl

The directory `inputs` contains a couple of sample input files.
The simplest way to try out `daedalus` is to use the interpreter to
parse a file.  For example:

    > daedalus run daedalus/formats/midi.ddl --input inputs/confuta.midi

This should print the AST for the sample midi file called `confuta.midi`.

You may generate a C++ parser by using the `compile-c++` command like this:

    > daedalus compile-c++ --out-dir=midi-parser daedalus/formats/midi.ddl

This should generate some C++ code in directory `midi-parser`:

    > ls midi-parser

You can build the generate parser using the makefile:

    > make -C midi-parser

This should generate an executable called `parser` in the `midi-parser` directory.
You can try out the compiled parser:

    > ./midi-parser/parser inputs/confuta.midi

This should print the AST again.  By default the C++ parsrer prints the
output in JSON.   The interpreter can also do that, if given the `--json` flag.

For more details on the Daedalus language, the tools, and a tutorial,
please have a look at our [online docuementation][6].


Running the Benchmarks
======================

Section 9.1 of our paper reports some benchmarks.  You may run the benchamraks
as follows:

    > cd /PLDI/daedalus
    > ./run-benchmarks

The benchmarsk should be already built, so you should just see `hyperfine`
doing the benchmarking.   To see more details about the benchmarks, have
a look at:

    formats/vlq_128
    formats/bson
    formats/s-exp

Each of these directories contains a sub-directory for each thing that
is being tested.

The data used for benchmarking is in a file called `data.dat` in each of
these directories.  These files are generated with a Haskell script
called `make-data.hs`.


The PDF Parser
==============

One of the big motivating examples for our work has been processing PDF
(Seciton 2).  The source code for our PDF parser is in:

    formats/pdf/new
      - pdf-cos-spec: specification of the COS structure of PDF
      - pdf-validate-spec: specifications for simple validation and
                           basics of text extractoin
      - c++: the build system and C++ code putting everything together

The executable for our PDF parser is avaiable `pdf-test` in the PLDI directory.
By default it performs some simple PDF validation, you can try out like this:

    > ./pdf-test inputs/confute.midi        # UNSAFE, REJECT
    > ./pdf-test inputs/sample.pdf          # SAFE, ACCEPT

To see some basic text extraction, use the `-t` flag:

    > ./pdf-test -t inputs/sample.pdf

At present our text extraction is pretty simple and does model text layout,
so you'll notice that there are not spaces between the extracted characters.


[1] https://www.docker.com/
[2] https://code.visualstudio.com/
[3] https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers
[4] https://code.visualstudio.com/docs/devcontainers/attach-container
[5] https://github.com/GaloisInc/daedalus
[6] https://galoisinc.github.io/daedalus/




