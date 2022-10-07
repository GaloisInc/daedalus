Getting Help
============

To see a list of available commands and options that apply to all commands:

.. code-block:: bash

  daedalus --help

To see a list of options for a specific command:

.. code-block:: bash

  daedalus COMMAND --help


Command ``show-types``
======================

To type-check a DaeDaLus specification and see the types of the declarations:

.. code-block:: bash

  daedalus show-types MyParserSpec.ddl

The command prints information about the types and parsers defined by
the given specification.  For example:

.. code-block:: none
  :caption: Sample output for ``show-types``

  Types
  ======

  def List a =
    union
      nil: {}
      cons: NonEmptyList a

  def NonEmptyList a =
    struct
      head: a
      tail: List a


  Parsers and Semantic Values
  ===========================

  def List:
    for any type a:
    parameter: parser of a
    defines: parser of List a

  def NonEmptyList:
    for any type a:
    parameter: parser of a
    defines: parser of NonEmptyList a

  def Main:
    defines: parser of List (uint 8)



The resulting types have the following form: 

.. code-block:: DaeDaLus

  def ParserName:
    parameter: <type A>
    parameter: <type B>
    ...
    defines: <type C>

This resembles a C type declaration as follows:

.. code-block:: C

  <type C> ParserName(<type A>, <type B>, ...); 

The types themselves may be simple types such as integers or arrays, but they
often have the form ``parser of <type A>``. This indicates that the parameter or
result is a parser, that itself generates semantic values of type ``A``. 

Command ``run``
===============

This command is used to run a DaeDaLus specification using the interpreter:

.. code-block:: bash

  daedalus run MyParserSpec.ddl --input=input.txt

This command will run the parser defined in ``MyParserSpec.ddl`` on the
input from file ``input.txt``.

If successful, the resulting semantic value will be shown on ``stdout``.

.. data:: --input=FILE

  Specifies that the input for the parser should be read from the given file.
  If no ``--input`` is specified, then the interpreter will run with
  an empty input.

.. data:: --entry=[MODULE.]NAME

  Specifies which parser defined by the specification should be executed.
  If no ``--entry`` is specified, then the interpreter will
  run the parser called ``Main``.

  The entry parser should have a fixed type (i.e., it should not be
  polymorphic) and it should not have any parameters, implicit or explicit.

.. data:: --json

  Show the parsed semantic valus using a JSON based format.

.. data:: --html

  Show the parsed semantic value rendered as HTML.


Command: ``compile-hs``
=======================

To compile a DaeDaLus parser specification to Haskell:

.. code-block:: bash

  daedalus compile-hs MyParserSpec.ddl

The result is a Haskell module which contains definitions for the
parsers and functions defined in the specification.

To use the generated code you'd have to integrate it with a Haskell
project and also use the ``rts-hs`` package.

.. warning::

  At present the command ``compile-hs`` uses the *old* Haskell backend which
  may go away in future.  To use the new backend, which is actively
  maintained, please use Template Haskell.   If there is deman, we may adapt
  the new backend to generate explcit Haskell files, in which case this
  command will start using the new backend.



Command: ``compile-c++``
========================

To compile a DaeDaLus package specification C++:

.. code-block:: bash

  daedalus compile-c++ MyParserSpec.ddl --out-dir=some_dir_name

This will generate a number of C++ files together with a ``Makefile`` and
a sample driver program in directory ``some_dir_name``.
The ``Makefile`` shows how to build the parser and has an option to
generate Doxygen documentation.

