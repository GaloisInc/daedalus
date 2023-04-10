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

Decisions on a boolean may be made either using :ref:`If-then-else`, by
using :ref:`Guards`, or by using :ref:`Case`.




Numeric Types
-------------

DaeDaLus supports a variety of numeric types: ``int``, ``uint N``, and
``sint N``, the latter two being families of types indexed by a number.
The type ``int`` classifies integers of arbitrary size.
The ``uint N`` classify unsigned numbers that can be represented using ``N``
bits and ``sint N`` is for signed numbers that can be represented
in ``N`` bits.


Numeric Literals
^^^^^^^^^^^^^^^^

Literals of the numeric types may written either using decimal or hexadecimal
notation (e.g., ``10`` or ``0xA``).  The type of a literal can be inferred
from the context (e.g., ``10`` can be used as both ``int`` a ``uint 8``).


Comparisons
^^^^^^^^^^^

Numeric types can also be compared for equality, using ``==`` and ordering
using ``<``, ``<=``, ``>``, and ``>=``.

Basic Arithmetic
^^^^^^^^^^^^^^^^

Numeric types support basic arithmetic: addition, subtraction, 
multiplication, division, and modulus using the usual operators
``+``, ``-``, ``*``, ``/``, and ``%``.

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


``maybe`` type
--------------

DaeDaLus supports the special polymorphic type ``maybe A``, which has possible 
values ``nothing`` and ``just x``, for some value, ``x`` of type ``A``.

The parser ``Optional P`` will try to parse the input using and produce
a ``maybe`` value.  If ``P`` succeeds with result ``x`` then
``Optional P`` will succeed with ``just x``, and if ``P`` fails, then
``Optional P`` will *succeed* with ``nothing``.

.. code-block:: DaeDaLus 

  def MaybeLetter = Optional $[ 'A'..'Z' ]

To examine values of the ``maybe`` type you may use
:ref:`Guards` or :ref:`Case`.


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

  -- Lookup an element in the map.
  lookup (key : ?k) (m : [ ?k -> ?v ]) : maybe ?v

  -- Lookup an element in the map.
  -- This is a parse which fails if the element is not in the map.
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

  -- Restrict a stream to the fist `n` bytes.
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






