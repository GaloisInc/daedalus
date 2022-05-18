Breaking down PPM: Expressions and Control Flow
===============================================

So far, we have introduced everything needed to understand the parsers defined
in the PPM example - but there are still some loose ends when it comes to
working with semantic values and control flow. This section will tie up those
ends, and give you the tools encessary to add some logic to your own DaeDaLus
specifications to enable more complex parsing and construction of semantic
values.

Built-In Semantic Values
------------------------

We've already discussed how user-defined structure and sum types can be defined
through parser specifications, but we haven't spoken in detail about what sorts
of data are available by default. It's about time we do that!

``bool``
^^^^^^^^

We've already mentioned the sum type ``bool``; it has precisely two values,
``true`` and ``false``. We can 'flip' a ``bool`` using the unary ``!``
operator. For example: If ``x`` is a ``bool`` that is ``true``, then ``!x`` is
``false``.

Two ``bool`` values can be combined using the operators ``&&`` and ``||``,
representing the usual logical notions of 'and' and 'or', respectively. It's
important to note that these are short-circuiting operators, meaning that the
two arguments may not both be evaluated if the final value can be determined
early.

Finally, two ``bool``s can be compared for equality with ``==``, and perhaps
strangely, are ordered according to ``false < true``.

Later in this section, we'll see the most common use of ``bool``s for control
flow via ``if``, guards, and pattern matching.

Numerics
^^^^^^^^

DaeDaLus supports unbounded integers with the type ``int``, and has two
*type families*, indexed by positive integers, to reprsent bounded integers.
Specifically, ``uint N`` (which we've seen in a few examples) is the type of
unsigned integers that can fit in ``N`` bits, and ``sint N`` is the type of
signed integers that can fit in ``N`` bits.

Numeric literals can be written in either decimal or hexadecimal notation, and
will have an appropriate type inferred based on the context they're used in.

As you might expect, numeric types support the usual collection of arithmetic
operations:

* Addition (``+``)
* Subtraction (``-``)
* Multiplication (``*``)
* Division (``/``)
* Modulus (``%``)

And comparison operations:

* Equality (``==``)
* Strict less-than (``<``)
* Less-than or equal (``<=``)
* Strict greater-than (``>``)
* Greater-than or equal (``>=``)

All numeric types also support bit-shifting operations ``<<`` and ``>>``; the
output type matches the input type, and the shift amount is a ``uint 64``.

Finally, the family of ``uint N`` types support bitwise operations, as these
types can also be interpreted as bitvectors (very useful for binary
specifications.) These are:

* Complement (``~``), which flips every bit
* Exclusive-or (``.^.``), which combines two same-sized bitvectors using the
  usual logical XOR operation on each pair of bits
* Bitwise-or (``.|.``), which combines two same-sized bitvectors using the
  usual logical 'or' operation on each pair of bits
* Bitwise-and (``.&.``), which combines two same-sized bitvectors using the
  usual logical 'and' operation on each pair of bits
* Non-truncating append (``#``), which combines two bitvectors by appending the
  bits of the second to the bits of the first. That is, given two bitvectors,
  one of type ``uint N`` and another of type ``uint M``, the result of ``#``
  between the vectors is a new bitvector of type ``uint (N + M)``
* Truncating append (``<#``) behaves like non-truncating append to combine two
  bitvectors of types ``uint N`` and ``uint M``, except only the ``N``
  least-significant bits are kept. The symbol used should remind you of the
  symbol used for biased choice when constructing parsers

``maybe ...``
^^^^^^^^^^^^^

The ``maybe T`` type represents values of type ``T``, with the possibility that
such a value is missing. It is a built-in sum type with two variants:
``nothing``, representing a 'missing' value, and ``just x``, where ``x`` is of
type ``T``. This type is very common in functional languages, provided as an
alternative to having something like a ``NULL`` value.

This type comes with a special parser, ``Optional``. The parser ``Optional P``
always succeeds: It returns ``just x`` if ``P`` succeeds and returns ``x``, and
it returns ``nothing`` if ``P`` fails.

Like ``bool``s, values of ``maybe`` type can be used in control flow via guards
and pattern matching, which we'll discuss shortly.

``[T]``
^^^^^^^

We have also already seen arrays in earlier sections, and how they can be built
through parser sequencing. We can also write array literals, however, using
familiar square-bracket notation from other languages:

* ``[]`` is an empty array, which takes on the type appropriate to the context
  in which it's used
* ``[1, 2, 3]`` is a 3-element array of integers; again, the type will be
  dependent on the context the integer literals are used in

The string literals we have seen are *also* array literals. For example,
``"hello"`` is an array of 5 bytes.

Besides the ``Many`` parser and the array sequencing syntactic sugar, there is
also the ``Index a i`` parser. This might seem like an odd one - typically,
accessing array elements wouldn't be considered 'parsing', after all. Indeed,
this being a parser is a clever trick to representing failure to index into an
array, when using an index that's out of bounds. Think of ``Index a i`` the
same as ``a[i]`` in other languages.

More in line with typical programming, the ``length`` semantic value is a
function that takes an array as argument and returns its length as a
``uint 64``.

In order to visit and use every element in an array, you can use a ``for``
loop, which we'll talk about in a later section.

Array Builders
^^^^^^^^^^^^^^

You may have noticed that there is no way to build arrays incrementally with
the tools outlined above. To address this, there is a special 'builder' type
that allows one to incrementally add elements to a structure that can then be
converted to an array.

The ``builder`` function returns a new, empty builder. ``emit b x`` takes a
builder of type ``builder T`` and a value of type ``T``, and adds that value
to the back of the builder, returning the new builder.

If instead you have an array you want to add to the back of a builder, you can
use the ``emitArray b xs`` function, which adds all the elements in the array
``xs`` to the back of the builder ``b``. Similarly, ``emitBuilder b1 b2`` adds
the builder ``b2`` to the back of ``b1``.

Once you're done building, you can use ``build b`` to convert the builder ``b``
into an array to be used as usual. Unlike arrays, builders do not have a direct
interface to lookup elements by index or compute length; they're useless
outside of incremental array construction.

``[K -> T]``
^^^^^^^^^^^^

Sometimes, we wish to index by something other than integers - this is
particularly useful when parsing formats that map names to other structures.

In DaeDaLus, the type of such an *association map* is written ``[K -> T]``,
where ``K`` is the key type and ``T`` is the element type. There is no literal
syntax for association maps - they must be built incrementally using a set of
functions or parsers.

First, ``empty`` returns a new, empty association map. Like ``[]``, this is a
polymorphic value that will take on a type appropriate for the context in which
it is used.

The function ``insert k v m`` inserts the key/value pair ``k/v`` into the map
``m``, returning a new map - if the key is already used, this function replaces
the original mapping.

If instead you'd like for failure to occur when a key is already defined, you
can instead use the parser version: ``Insert k v m``.

Finally, there are two ways to look up a key in a map: The function version,
``lookup k m``, returns ``nothing`` if the key ``k`` is not defined, and
``just v`` if it is. Like with insertion, if you'd rather trigger a failure
when lookup fails, you can use the parser version: ``Lookup k m``, which
returns the element itself rather than wrapping it in a ``maybe``.
