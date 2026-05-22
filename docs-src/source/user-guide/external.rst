External Declarations
=====================

DaeDaLus supports an interface for declaring parsers and types whose
implementations are provided by the code generation backend (currently
C++).  This is useful when integrating DaeDaLus-generated code with
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
