DaeDaLus
--------


Setting up Your Editor
======================

Directory ``syntax-hilight`` contain Daedalus syntax hilighting files for
common editors.


Syntax
======

Identifiers
~~~~~~~~~~~
There are four classes of identifier in DaeDaLus:
 1. Built-in keywords, such as ``Many``
 2. Names of character sets, which begin with ``$``
 3. Names of parsers, which begin with uppercase English letters
 4. Names of fields and local variables, which begin with lowercase English letters


Types
=====

Daedalus has a largely structural type system that features records,
variants, and sequences, in addition to base types like fixed-width
integers. There's presently no concrete syntax for adding type
ascriptions - types are inferred.

Operations
==========

The primary constructs of the language are:
* Parsers, which can be understood as a monadic language
* Byte sets, built up from byte literals, byte ranges, and union
* Pure computations, like arithmetic expressions

Depending on context, syntactically identical expressions can be
interpreted in different ways. For instance, a byte literal is a
singleton set in a byte set context, a parser that matches only that
byte in a parser context, and a value in a pure computation context.

``as``
  In a pure context, this operator coerces a value from one type to
  another, but is a static error when the type being coerced to cannot
  necessarily represent the value. In a parser context, the parser
  fails when the value cannot be coerced.

``^``
  The unit of the parsing monad, analogous to Haskell's ``pure`` or
  ``return``.

``@``
  Before a field name in a parsing record, this operator causes the
  parsed value to not be included as a field. The name is nonetheless
  brought into scope, and the value that results from the parser
  action can be used in subsequent parsers.

``!``
  Applied to a Boolean, this unary prefix operator means
  negation. Applied to a byte set, it represents the complement of the
  byte set.

Primitive Functions
~~~~~~~~~~~~~~~~~~~

``(#) : uint m -> uint n -> uint (m + n)``
  Appends two bit vectors. The first one is in the more significant
  part of the result.

``(<#) : Numeric a => a -> uint n -> a``
  ``x <# y`` is similar to ``#`` but uses the type of the first
  argument. Operationally, this shifts ``x`` to the left by ``n`` and
  ors the result with ``y``. This captures a common use case of
  appending things in a result. Note that this could lose information.

``(<<), (>>) : Numeric a => a -> int -> a``
  Shift left or shift right. We should probably generalize over the
  ``int`` but to avoid ambiguit we keep it like this for the moment. The
  issue is similar to the bounds argument of ``Many``.

``(.&.), (.|.), (.^.) : uint n -> uint n -> uint n``
  Bitwise and, or, xor.

``~ : uint n -> uint n``
  Bitwise complement.
