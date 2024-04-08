Getting Started
===============

We provide a Docker image which contains a development environment including:
  * `daealus`: the parser generator described in our paper
  * General development tools for working with Haskell and C++ code.
  * Other parser generators we use for benchmarking, namely:
    - antlr
    - kaitai-struct-compiler

To use the image you'll need [docker][1]:

    > docker image load -i daedalus-pldi
    > docker run -it daedalus:08-March-2024 /bin/bash

This should start the image, attach to the running container, and
start a shell on the terminal.  Once you are in the container image,
you should see a directory called `daedalus`:
this is the source code repository for our tool,
which is also available on [github][2].

One way to try out the Daedalus language server capabilities is to
use VS Code, which can be setup like this:

  1. Install the VS Code "Remote Development" extension pack,
   which contains extensions for working with docker containers.
  2. Run the `docker` image as above (`docker run ...`)
  3. From within VS Code: click the button in the bottom left corner
     that has a `><` symbol on it ("Open a Remote Window").
  4. This will pop up a window with options,
     select "Attach to a Running Container.."
  5. This will show some docker containers you have (possibly just one).
     Select the one that has `daedalus:08-March-2024` in the name.
  6. This should start a new VS Code window, which allows you to
     to work inside the container.
  7. From the VS Code instance that is running in the container:
      7.1 Click on the extensions tab (has some squares on it)
      7.2 Open the menu in the top right (3 dots)
      7.3 Select "Install from VSIX"
      7.4 Select the following location: `/usr/local/lib/daedalus-0.0.1.vsix`
  8. To try out the language integration open a daedalus file:
      8.1 For example: `/PLDI/daedalus/formats/midi.ddl`
      8.2 You should see some syntax highlighting
      8.3 You can hover over things to see their types
      8.4 If you make a mistake, you should see some errors.


Example Daedalus Specifications
===============================

There are some example specifications, in various degrees of completeness,
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

This should print the AST again.  By default the C++ parser prints the
output in JSON.   The interpreter can also do that, if given the `--json` flag.

For more details on the Daedalus language, the tools, and a tutorial,
please have a look at our [online documentation][3].

The `daedalus` interpreter can render the parsed result in HTML form,
where the parsed values are annotated with their position in the input.
You can see an example of this like this:

    > cd /PLDI
    > daedalus run --traced-values --html               \
        daedalus/formats/midi.ddl                       \
        --input=inputs/confuta.midi > example.html

This should generate a file called `example.html` that you can open
with a browser and you should see the parsed document on the right.
You can click on the various parts of the document and it should highlight
the ranges in the input that contributed towards parsing that value.



Running the Benchmarks
======================

Section 9.1 of our paper reports some benchmarks.  You may run the benchmarks
as follows:

    > cd /PLDI/daedalus
    > ./run-benchmarks

The benchmark should be already built, so you should just see `hyperfine`
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
(Section 2).  The source code for our PDF parser is in:

    formats/pdf/new
      - pdf-cos-spec: specification of the COS structure of PDF
      - pdf-validate-spec: specifications for simple validation and
                           basics of text extraction
      - c++: the build system and C++ code putting everything together

The executable for our PDF parser is available `pdf-test` in the PLDI directory.
By default it performs some simple PDF validation, you can try out like this:

    > ./pdf-test inputs/confuta.midi        # UNSAFE, REJECT
    > ./pdf-test inputs/sample.pdf          # SAFE, ACCEPT

To see some basic text extraction, use the `-t` flag:

    > ./pdf-test -t inputs/sample.pdf

At present our text extraction is pretty simple and does not model text layout,
so you'll notice that there are not spaces between the extracted characters.


[1] https://www.docker.com/
[2] https://github.com/GaloisInc/daedalus
[3] https://galoisinc.github.io/daedalus/


Trying Out Talos
================

`talos` is an experimental tool that can generate inputs for a subset
of Daedalus grammars.   One may try it out as follows:

    1. Install `z3`:  apt-get install z3

    2. Build and install `talos`:
        2.1 cd /PLDI/daedalus
        2.2 cabal build talois
        2.3 cp $(cabal exec which talos) /usr/local/bin

    3. Test `talos`:
        3.1 Make a sample grammar file, for example:

            (in a file called `test.ddl`)
            def Main =
              block
                Match "MAGIC"
                Many (UInt8 as uint 64) $['a' .. 'z']

        3.2 talos test.ddl
        3.3 You can run it multiple times to get different inputs.



