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

Flags
-----

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

.. data:: --core

  Use the Core IR interpreter.

.. data:: --vm

  Use the VM IR interpreter.


Command: ``compile-hs``
=======================

To compile a DaeDaLus parser specification to Haskell:

.. code-block:: bash

  daedalus compile-hs MyParserSpec.ddl --out-dir=some_dir_name

The result is a directory populated with a Haskell module containing
definitions for the parsers and functions defined in the specification.
In addition, the DaeDaLus compiler will generate a sample executable
driver and Cabal package description for easy prototyping. To use the
generated code you'll have to compile it with the ``rts-hs`` package
provided in this distribution.

.. warning::

  At present the command ``compile-hs`` uses the *old* Haskell backend which
  may go away in future.  To use the new backend, which is actively
  maintained, please use Template Haskell.   If there is demand, we may adapt
  the new backend to generate explicit Haskell files, in which case this
  command will start using the new backend.

Command: ``compile-c++``
========================

To compile a DaeDaLus package specification C++:

.. code-block:: bash

  daedalus compile-c++ MyParserSpec.ddl --out-dir=some_dir_name

Generated Files
---------------

The compiled parser is in two files:
  * ``main_parser.h`` contains the interface to the generated parser, and
  * ``main_parser.cpp`` contains the implementation of the parser.

In addition, the DaeDaLus compiler will generate a sample executable driver
and ``Makefile`` illustrating how to build the parser and generate Doxygen
documentation.  The sample executable expects that the compiled specification
contains a parser named ``Main`` that has a fixed type and no parameters
of any kind---if this is not the case, the sample driver will fail to compile.

The sample executable driver will be generated when:

  * there is no explicit ``--entry`` specified, and
  * there is no custom user state ``--user-state``

Flags
-----


.. data:: --out-dir=DIRECTORY

  Save generated files in the given directory.
  The directory will be created if it does not exist.

.. data:: --out-dir-header=DIRECTORY

  Save generated header files in the given directory.
  The directory will be created if it does not exist.
  If this is not specified, then the headers will be saved in
  ``--out-dir``

.. data:: --file-root=STRING

  Use the given string for the names of the generated parser
  implementation and header.
  If not specified this will be ``main_parser``.

.. data:: --user-namespace=NAMESPACE

  Place generated type declarations in the given name space.
  Note that at present this applies only to generate types.
  Generated parser entry point always reside in the default namespace.
  If this is not specified, this will be ``User``

.. data:: --no-error-stack

  Disable call stack tracking in errors.   Without this option,
  we generate code that leads to more detailed parse error messaged,
  at some runtime cost.   Adding this flag leads to less detailed
  parse errors, but some potential performance gain.

.. data:: --user-state=SATE_TYPE

  Generate a parser where the parser's state will be extended with
  some user state of the given type.  This is useful in certain situations
  (e.g., caching of data), but care needs to be taken that updates
  to this state make sense even when the parsers backtracks.

.. data:: --add-include=INCLUDE

  Add an additoinal `#include` to the generated parser header file.
  Typically, this is used to support custom user state, or integration
  with an pre-compiled DaeDaLus parser.  The parameter should just contain
  the thing to be incuded (e.g., ``"file.h"`` or ``<file.h>``).

.. data:: --entry=[MODULE.]NAME

  Declare that the given parser is an entry point for the generated parser.
  Specifying an entry point will disable the genration of a sample driver
  executable.  The name of the C++ function corresponding to DaeDaLus
  declaration ``F`` is ``parseF``.  The entry point should have a fixed
  type, but MAY have argument.

  .. code-block:: C++
    :caption: Sample entry point for parser ``Main`` with no extra arguments,
              returning a semantic value of type ``T``.

    void parseMain ( DDL::ParseError &error
                   , std::vector<T>> &results
                   , DDL::Input      input
                   );

  If an extry point has arguments, they'll come after the ``input`` argument.
  The entry point returns any parsed semantic values in the ``results`` vector,
  there could be multiple results if the grammar is ambiguous.  If there
  are no results (i.e., ``results`` is empty), then ``error`` will contain
  information about the parser error.

.. data:: --inline

  Perform agressive inlinining---all parsers that can be inlined will be
  inlinined at each use site.  Sometimes this may improve performance, but
  sometimes it may reduce it, due to a significant increase in the size
  of the generate code.

.. data:: --inlinine-this=[MODULE.]NAME

  Inline uses of a specific parser.

.. data:: --extern=MODULE[:NAMESPACE]

  Do not generate declarations for the types declared in the given module.
  This is useful when interfacing with a pre-compiled DaeDaLus parser,
  which already contains declarations for the given types.  Often, this
  option will be comined with ``add-include`` to include the declarations
  of the already compiled code.  If a namespace is provided, then uses of
  types in the given module will be qualified with the given namespace.
  Otherwise, the default namespace ``User`` will be used.


Command: ``dump``
=================

It shows the various IR structures of the DaeDaLus compiler. This
command is largely for debugging.

.. data:: --parsed

  Show the results of just parsing a specification.  This ensures that
  a specification is syntactically correct.

.. data:: --resolved

  Show the results of name resultion.  This associates uses of names
  with their definitions.

.. data:: --tc

  Show the results of type-checking.  This validates the specification
  and esnures that parsers are used correctly (e.g. have the correct
  number of arguments with the correct types).  The ``dump`` command
  will show this phase, unless another is specified.

.. data:: --spec

  Show the IR after *specialization*.  Specialization eliminates
  polymorphism and parsers with other parsers as arguments.
  This works by generating custom instances of a parser
  (similar to C++ template expansion).  After this pahse,
  parsers are monomorphic and have only semantic values as parameters.

.. data:: --core

  Show the Core IR representation.  A number of DaeDaLus constructs
  are desugared into simpler constructs.

.. data:: --vm

  This is the lowest level IR, which is used to generate code.
  It is essentially a control flow graph in SSA form, except that
  instead of using "phi" blocks, basic blocks have parameters.
