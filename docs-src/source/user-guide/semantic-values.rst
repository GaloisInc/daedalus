Semantic Values
===============

If successful, a parser produces a semantic value, which describes the
input in some way useful to the application invoking the parser.
In addition, semantic values may be used to control how other parts of the
input are to be parsed.  DaeDaLus has a number of built-in semantic values
types, and allows for user-defined record and union types.

Booleans
--------

The type ``bool`` classifies the usual boolean values ``true`` and ``false``.

The operator ``!`` may be used to negate a boolean value.
The operators ``||`` and ``&&`` are for (short-circuiting) "or" and "and"
respectively.

Boolean values may be compared for equality using ``==`` and are ordered
with ``false < true``.

Decisions on a boolean may be made either using :ref:`user-guide/control-structures:If-then-else`, by
using :ref:`user-guide/control-structures:Guards`, or by using :ref:`user-guide/control-structures:Case`.




Numeric Types
-------------

DaeDaLus supports the following numeric types:

=========== ===========================================================
Type        Description
=========== ===========================================================
``int``     Integers of arbitrary size
``uint N``  Unsigned integers represented in ``N`` bits
``sint N``  Signed integers represented in ``N`` bits
``float``   IEEE 754 single-precision (32-bit) floating-point numbers
``double``  IEEE 754 double-precision (64-bit) floating-point numbers
=========== ===========================================================


Numeric Literals
^^^^^^^^^^^^^^^^

Integer literals may be written in decimal, hexadecimal (``0x``),
octal (``0o``), or binary (``0b``) notation (e.g., ``10``, ``0xA``,
``0o12``, ``0b1010``).  The type of a literal can be inferred from the
context (e.g., ``10`` can be used as both ``int`` and ``uint 8``).

There is also a byte-string numeric literal written as ``0s"..."``, which
interprets each character as a byte (8 bits) and concatenates them into
a single integer.  For example, ``0s"scnr"`` is a ``uint 32`` with value
``0x73636E72``.  The width of the resulting type is the number of
characters times 8.

Floating-point literals are written with a decimal point (e.g., ``3.14``).
The constant ``pi`` is also available at both ``float`` and ``double`` types.


Comparisons
^^^^^^^^^^^

Numeric types can also be compared for equality using ``==`` and ``!=``,
and ordered using ``<``, ``<=``, ``>``, and ``>=``.

Basic Arithmetic
^^^^^^^^^^^^^^^^

All numeric types support addition, subtraction, multiplication, and
division using the operators ``+``, ``-``, ``*``, and ``/``.
The modulus operator ``%`` is available only for integer types.

For floating-point types, arithmetic follows IEEE 754 semantics and never
raises an exception---operations that overflow or are undefined produce
infinity or NaN instead.

Arithmetic Exceptions
^^^^^^^^^^^^^^^^^^^^^

Some arithmetic operations can raise exceptions on integer types:

* ``+``, ``-``, ``*``, and unary negation raise an exception if the
  result overflows for ``uint N`` or ``sint N``.  Operations on ``int``
  never overflow.
* ``/`` and ``%`` raise an exception on division by zero.  They can also
  raise an exception on ``sint N`` when dividing the minimum value by -1
  (as the result overflows).
* ``<<`` and ``>>`` raise an exception if the shift amount is too large
  (when the first operand is of type ``int``).

Arithmetic exceptions cannot be caught and will result in the parser
terminating with a parse error.

Bitwise Operations
^^^^^^^^^^^^^^^^^^

DaeDaLus also supports shift operations ``<<`` and ``>>``.
These operations are overloaded and can be used on all numeric types,
with the restriction that the inputs and the outputs must be of the
same type.  The shift amount is a value of type ``uint 64``.

Unsigned integers may also be treated as bit-vectors, and support various
bitwise operations:

* complement: ``~``
* bitwise exclusive-or ``.^.``
* bitwise-or ``.|.``
* bitwise-and ``.&.``.

Unsigned numbers can also be appended to other numbers via the
``#`` and ``<#`` operator.  To see the difference between the two,
consider two bitvectors ``(x : uint A)`` and ``(y : uint B)``.
The result of ``x # y`` is a bitvector of type ``A + B`` with
``x`` in the more significant bits, and ``y`` in the less significant bits.
The result of ``x <# y`` is a bitvector of type ``A`` that contains
``x # y`` but truncated to the ``A`` less significant bits.


Floating-Point Operations
^^^^^^^^^^^^^^^^^^^^^^^^^

Comparisons on floating-point types follow IEEE 754 semantics:
comparisons involving NaN return ``false``, except for ``!=`` which
returns ``true`` when either operand is NaN.

The following predicates test for special IEEE 754 values:

========================= ===================================================
Predicate                 Description
========================= ===================================================
``isNaN x``               True if the value is NaN
``isInfinite x``          True if the value is positive or negative infinity
``isDenormalized x``      True if the value is denormalized
``isNegativeZero x``      True if the value is negative zero
========================= ===================================================

The following operations reinterpret the bits of an integer as a
floating-point value (a bitcast, not a numeric conversion):

================================== ==========================================
Operation                          Description
================================== ==========================================
``wordToFloat (x : uint 32)``      Reinterpret 32 bits as a ``float``
``wordToDouble (x : uint 64)``     Reinterpret 64 bits as a ``double``
================================== ==========================================


Coercions Involving Floating-Point Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Integer types may be coerced to floating-point types.  The coercion
is lossless (``as``) only if the integer is exactly representable;
otherwise use ``as!`` (which may round) or ``as?`` (which fails if
rounding would occur).

Coercion from ``float`` to ``double`` is always exact.  Coercion from
``double`` to ``float`` may lose precision; use ``as?`` to detect this.

When coercing from a floating-point type to an integer type using ``as!``,
the value is truncated toward zero and clamped to the target range:

=================== =========================================================
Value               ``int``        ``uint N``            ``sint N``
=================== =========================================================
NaN                 0              0                     0
Positive infinity   0              2^N - 1               max representable
Negative infinity   0              0                     min representable
Normal values       truncated      clamped to [0, 2^N-1] clamped to range
=================== =========================================================

The ``as?`` coercion succeeds only when the conversion is exact (i.e.,
the floating-point value is an integer that fits in the target type).


``maybe`` type
--------------

DaeDaLus supports the special polymorphic type ``maybe A``, which has possible 
values ``nothing`` and ``just x``, for some value, ``x`` of type ``A``.

The parser ``Optional P`` will try to parse the input using ``P`` and produce
a ``maybe`` value.  If ``P`` succeeds with result ``x`` then
``Optional P`` will succeed with ``just x``, and if ``P`` fails, then
``Optional P`` will *succeed* with ``nothing``.

.. code-block:: DaeDaLus 

  def MaybeLetter = Optional $[ 'A'..'Z' ]

To examine values of the ``maybe`` type you may use
:ref:`user-guide/control-structures:Guards` or :ref:`user-guide/control-structures:Case`.


Arrays
------

The type of arrays containg elements of type ``T`` is ``[T]``.

.. code-block:: DaeDaLus

  -- Array literals
  []        -- empty array
  [1,2,3]   -- array with 3 elements
  "Hello"   -- array with 5 elements

  -- Get the element at the given array index
  -- This is a parser, which fails if the index is out of bounds
  Index (a : [?a]) (i : uint 64) : ?a

  -- Length of an array
  length (a : [?a]) : uint 64

To visit all elements in array you may use a ``for`` loop :ref:`for_loops`.


Numeric Ranges
^^^^^^^^^^^^^^

The ``rangeUp`` and ``rangeDown`` operations produce arrays of numbers:

.. code-block:: DaeDaLus

  rangeUp end                -- [0, 1, ..., end-1]
  rangeUp start end          -- [start, start+1, ..., end-1]
  rangeUp start end step     -- [start, start+step, ...] while < end

  rangeDown start            -- [start, start-1, ..., 1]
  rangeDown start end        -- [start, start-1, ..., end+1]
  rangeDown start end step   -- [start, start-step, ...] while > end

The step must be a positive number; a non-positive step raises an
exception.  Some examples:

.. code-block:: DaeDaLus

  rangeUp 5           -- [0, 1, 2, 3, 4]
  rangeUp 10 20       -- [10, 11, 12, ..., 19]
  rangeUp 10 20 3     -- [10, 13, 16, 19]
  rangeDown 5         -- [5, 4, 3, 2, 1]
  rangeDown 20 10     -- [20, 19, 18, ..., 11]
  rangeDown 20 10 3   -- [20, 17, 14, 11]

.. warning::

  These operations construct the entire array in memory.  They are not
  lazy iterators, so avoid using them for very large ranges.


Array Builders
--------------

A ``builder`` is a datastructure that helps build arrays.
To build an array, start with the empty builder ``builder`` and use
``emit`` to add elements to the builder.  Once all elements have been
added, you may use ``build`` to convert the ``builder`` to an array.

.. code-block:: DaeDaLus

  -- empty builder
  builder : builder ?a

  emit (front : builder ?a) (back : ?a) : builder ?a

  -- Add an array of element to the end of the builder
  emitArray (front : builder ?a) (back : [?a]) : builder ?a

  -- Add a builder at the end of another builder
  emitBuilder (front : builder ?a) (back : builder ?a) : builder ?a

  -- Turn a builder into an array
  build (b : builder ?a) : [?a]



Association Maps
----------------

The type of association maps with keys of type ``K`` and elements of type
``T`` is ``[ K -> T ]``.

.. code-block:: DaeDaLus

  -- An empty map
  empty : [ ?k -> ?v ]

  -- Insert an element in a map.
  -- The element is replaced, if it was already present.
  insert (key : ?k) (value : ?v) (m : [ ?k -> ?v ]) : [ ?k -> ?v ]

  -- Insert an element in a map.
  -- This is a parser which fails if the element was already in the map.
  Insert (key : ?k) (value : ?v) (m : [ ?k -> ?v ]) : [ ?k -> ?v ]

  -- Look up an element in the map.
  lookup (key : ?k) (m : [ ?k -> ?v ]) : maybe ?v

  -- Look up an element in the map.
  -- This is a parser which fails if the element is not in the map.
  Lookup (key : ?k) (m : [ ?k -> ?v ]) : ?v

To visit all elements of an association map you may use a
``for`` loop :ref:`for_loops`.


Streams
-------

The type ``stream`` is for values representing streams of data that
can be parserd by a parser.   See :ref:`stream_manipulation` for more examples
of how to manipualte the parser's stream.

.. code-block:: DaeDaLus

  -- Get the current stream for the parser
  GetStream : stream

  -- Restrict a stream to the first `n` bytes.
  -- Will fail if the stream does not have enough bytes
  Take (n : uint 64) (s : stream) : stream

  -- Restrict a stream to at most `n` bytes.
  -- The resulting stream might be shorter if there are not
  -- enough bytes
  take (n : uint 64) (s : stream) : stream

  -- Advance a stream by `n` bytes.
  -- Will fail if the stream does not have enough bytes
  Drop (n : uint 64) (s : stream) : stream

  -- Make a stream with the given name and bytes to parser
  arrayStream (name : [uint 8]) (data : [uint 8]) : stream

  -- Get the bytes associates with a stream as an array
  bytesOfStream (s : stream) : [uint 8]






