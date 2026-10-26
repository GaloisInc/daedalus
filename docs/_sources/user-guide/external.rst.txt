External Declarations
=====================

DaeDaLus supports an interface for declaring parsers and types whose
implementations are provided by the code generation backend (currently
C++ and Rust).  This is useful when integrating DaeDaLus-generated code with
host-language libraries or when certain operations cannot be expressed
in DaeDaLus itself.

External Primitives
-------------------

A declaration with a type signature but no body declares an external
primitive:

.. code-block:: DaeDaLus

  def SetSpecial (x : uint 8) : {}
  def GetSpecial : uint 8

This tells DaeDaLus that these parsers exist with the given types, but
their implementations must be provided by the code generation backend.
External primitives may not use implicit parameters.

When generating C++, the code generator emits an ``extern`` declaration
for each external primitive.  The user must provide a corresponding
implementation.  For a parser returning type ``T`` with parameters
``p1, ..., pN``, the C++ signature is:

.. code-block:: c++

  DDL::ParserResult parser_Name(
      DDL::ParserState& state,
      T* result, DDL::Input* newInput,
      DDL::Input currentInput, P1 p1, ..., PN pN);

The function should return ``DDL::ParserResult::Ok`` on success (setting
``*result`` and ``*newInput``), ``DDL::ParserResult::Failure`` on parse
failure, or ``DDL::ParserResult::Exception`` for unrecoverable errors.

Reference-typed arguments (arrays, maps, streams, etc.) are passed as
*owned* values.  This means that on failure the external function is
responsible for deallocating them.  On success, ownership of the arguments
is considered transferred and the caller will not free them.

Here is a complete example:

.. code-block:: c++

  // Implementation of: def GetSpecial : uint 8
  DDL::ParserResult parser_GetSpecial(
      DDL::ParserStateUser<DDL::Input, State>& state,
      DDL::UInt<8>* result, DDL::Input* newInput,
      DDL::Input currentInput) {
    *result = DDL::UInt<8>(state.getUserState().get());
    *newInput = currentInput;
    return DDL::ParserResult::Ok;
  }

When generating Rust, use ``--user-fun=QUAL`` to specify the module
containing implementations of external functions.  The code generator emits
will use ``QUAL::f`` when calling external function ``f``.
Additional Rust imports may be added with ``--add-import``.

If an external function needs to use types defined by the generated parser,
generate the parser directly into the application crate with
``--output-file``.  The generated parser and the native implementation can
then be sibling modules, allowing the native implementation to import
generated types through paths such as ``crate::format::Packet``.

External Module Imports
-----------------------

When importing a module, the ``extern`` keyword indicates that the
code generator should not produce type definitions for that module:

.. code-block:: DaeDaLus

  import extern MyTypes

This is useful when compiling multiple parser specifications separately
that share common type definitions.  Without ``extern``, each compiled
specification would generate its own copy of the shared types, leading
to duplicate definitions.  With ``import extern``, one specification
generates the type definitions and the others simply reference them
without generating them again.
