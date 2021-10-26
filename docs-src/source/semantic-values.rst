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
``+``,``-``,``*``,``/``, and ``%``.

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

.. todo::
  Describe the interface to ``[ el_type ]``

Association Maps
----------------

.. todo::
  Describe the interface to ``[ key -> value ]``


