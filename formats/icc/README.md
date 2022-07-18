Overview
========

This directory contains a proof of concept for a tool for analyzing documents
in the ICC format.  The format of ICC is based on the following documents:

    https://www.color.org/specification/ICC.2-2019.pdf
    https://www.color.org/icc32.pdf

The Daedalus version of the specification is located in the `spec` subdirectory:

    # Core specification for ICC
    spec/ICC.ddl

    # A wrapper for running the parser using the Daedalus interprter.
    spec/Parser.ddl

    # A validator that analyzes the calculator elements in an ICC document.
    spec/Validator.ddl


Running the Tool
================

There are two ways to use the ICC specifications:

  1. Execute the specifications using a DaeDaLus interpreter, or
  2. Use the provided Haskell wrapper to compile the DaeDaLus specifications
     to an executable.

The benefit of option (1) is a quicker edit-run cycle, which is handy if
you are modifying the specification frequently.  The drawbacks are:
  (i)  slower performance, due to interpretive overhead, and
  (ii) the output uses DaeDalus's generic output format to print the results,
       which can be hard to read

The benefit of option (2) is faster performance, but more importantly, the
Haskell wrapper comes with a custom pretty printer, which makes the output
easier to read.


Running the Spec Using the Interpreter
--------------------------------------

Assuming that `daedalus` is in the path:

    # To see the parsed AST for MY_ICC_FILE
    daedalus run spec/Parser.ddl --input=MY_ICC_FILE

    # To validate calc elements in MY_ICC_FILE
    daedalus run spec/Validator.ddl --input=MY_ICC_FILE

If you don't have a pre-built interpreter, but are working from within
the DaeDaLus repository, you can build the `daedalus` interpreter like this:

    # Build `daedalus` interpreter (only need to do once)
    cabal build daedalus

This will build the interpreter and store it in a build specific location.
You can use this interpreter as follows:

    cabal exec daedalus -- run spec/Parser.ddl --input=MY_ICC_FILE
    cabal exec daedalus -- run spec/Validator.ddl --input=MY_ICC_FILE

This is very similar to the above commands, except `daedalus` is replaced
with `cabal exec daedalus --`.    You can find the location of the
`daedalus` binary using:

    cabal exec which daedalus

If you wish you may copy the binary somewhere for later use.


Running the Spec Using the Compiled Haskell Wrapper
---------------------------------------------------

To use the provided Haskell wrapper, you need to compile it first, and you will
need to recompile it if you make changes to the ICC specs or the pretty printer.

    # Build ICC tool
    cabal build icc

    # Find out the location of `icc` if you want to copy it:
    cabal exec which icc

To run the tool you can use:

    # To see the AST
    icc --ast MY_ICC_FILE                 # if `icc` is in the path
    cabal exec icc -- --ast MY_ICC_FILE   # to use the `icc` in the build system

    # To run the validator
    icc MY_ICC_FILE                       # if `icc` is in the path
    cabal exec icc -- MY_ICC_FILE         # to use the `icc` in the build system


Modifying the Pretty Printer
----------------------------

The `icc` pretty printer is located in:

    src/PP.hs

If you change the ICC spec, you'll likely want to update this to say
how to print things.























