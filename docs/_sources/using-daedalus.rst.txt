
Help
====

To see a list of options:

.. code-block:: bash

  daedalus --help

Check a Specification
=====================

To type-check a DaeDaLus specification and see the types of the declarations:

.. code-block:: bash

  daedalus --show-types MyParserSpec.ddl

The resulting types have the following form: 

.. code-block:: DaeDaLus 

  ParserName: 
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

Run the Interpreter
===================

To run interpret a specification on a particular input:

.. code-block:: bash

  daedalus MyParserSpec.ddl --interp=input.txt

If successful, the resulting semantic value will be shown on ``stdout``.
You may also add the flag ``--json`` or ``--html`` to see the semantic value
in the corresponding formats.

.. todo::

  Document the JSON schema


Compile to Haskell
==================

To compile a DaeDaLus parser specification to Haskell:

.. code-block:: bash

  daedalus MyParserSpec.ddl --compile-hs

The result is a Haskell module which contains definitions for the
parsers and functions defined in the specification.   To use the generated
code you'd have to integrate it with a Haskell project and also use
the ``rts-hs`` package.


Compile to C++
==============

To compile a DaeDaLus package specification C++ 17:

.. code-block:: bash

  daedalus MyParserSpec.ddl --compile-c++ --out-dir=some_dir_name

This will generate a number of C++ files together with a ``Makefile`` and
a sample driver program in directory ``some_dir_name``.
The ``Makefile`` shows how to build the parser and has an option to
generate Doxygen documentation.

