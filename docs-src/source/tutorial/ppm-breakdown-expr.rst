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

We've already mentioned the sum type ``bool``: it has precisely two values,
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

.. todo

    Are right-shifts arithmetic or logical for signed types?

Finally, the family of ``uint N`` types support bitwise operations, as these
types can also be interpreted as bitvectors (which is very useful for binary
specifications.) These are:

* Complement (``~``), which flips every bit
* Exclusive-or (``.^.``), which combines two same-sized bitvectors using the
  usual logical 'xor' operation on each pair of bits
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
this being a parser is a clever trick to represent failure to index into an
array, when using an index that's out of bounds. Think of ``Index a i`` the
same as ``a[i]`` in other languages.

More in line with typical programming, the ``length`` semantic value is a
function that takes an array as argument and returns its length as a
``uint 64``.

In order to visit and use every element in an array, you can use a ``for``
loop, which we'll talk about in a later section.

Builders for Arrays
^^^^^^^^^^^^^^^^^^^

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
functions and parsers.

First, ``empty`` returns a new, empty association map. Like ``[]``, this is a
polymorphic value that will take on a type appropriate for the context in which
it is used.

The function ``insert k v m`` inserts the key/value pair ``k/v`` into the map
``m``, returning a new map - if the key is already used, this function replaces
the original mapping.

If instead you'd like for failure to occur when a key is already defined, you
can instead use the parser version: ``Insert k v m``.

Finally, there are two ways to look up a key in a map. The function version,
``lookup k m``, returns ``nothing`` if the key ``k`` is not defined, and
``just v`` if it is. Like with insertion, if you'd rather trigger a failure
when lookup fails, you can use the parser version: ``Lookup k m``, which
returns the element itself rather than wrapping it in a ``maybe``.

Control Strucutres
------------------

With a solid understanding of the types of values we have available, it's now
time to see how they're used to control parser behaviors (and fill in all the
gaps we left above!)

``if ... then ... else``
^^^^^^^^^^^^^^^^^^^^^^^^

We can use a ``bool`` value to control which of two parsers runs. This is not
used in the PPM example, but here is a simple example that parses an ``'A'`` if
the first parsed digit is less than 5, and ``'B'`` otherwise:

.. code-block:: DaeDaLus

    @i = Match1 ('0' .. '9')
    if (i - '0') < 5
      then Match1 'A'
      else Match1 'B'

Guarding
^^^^^^^^

When writing a complex parser, it is often useful to confirm that the 'shape'
of some value we've parsed from input is correct; this is the job of a *guard*.

Guards are parsers that succeed when a given expression has a given shape. They
can be used with ``bool``s, ``maybe`` values, and generic tagged sums built
from alternatives parsers.

An example comes from the PPM example:

.. code-block:: DaeDaLus

    def PPM = {
      Match "P";
      @version = Token Natural;
      version == 3 is true;
      width  = Token Natural;
      height = Token Natural;
      maxVal = Token Natural;
      data   = Many height (Many width RGB);
    }

Here, the line ``version == 3 is true`` is a guard. No input is consumed at
this line, but if the expression does not evaluate to true, it triggers a parse
failure.

If ``e`` is a ``maybe``-typed value, we can use the guards ``e is just`` or
``e is nothing``. The same pattern works for user-defined tagged sums that
arise from parsing alternatives using biased/unbiased choice.

``case ... of ...``
^^^^^^^^^^^^^^^^^^^

A more generic way to inspect a semantic value, in particular a tagged sum, is
the *pattern-matching* structure, ``case ... of ...``.

In general, a ``case`` expression looks like:

.. code-block:: DaeDaLus

    case e of
      p1 -> b1
      p2 -> b2
      ...

Where ``e`` is an expression, and the ``pi``s are *patterns*. The patterns are
checked in order, and the first that matches the shape of ``e`` has its body
evaluated.

Here's an example that uses something like the ``GoodOrBad`` type from earlier:

.. code-block:: DaeDaLus

    block
      @res = Choose
               good = Match1 'G'
               bad  = Match1 'B'
      case res of
        good -> ^ "Good!"
        bad  -> ^ "Bad!"

If the variants of our tagged sum have arguments, our patterns may give these
arguments names so that they may be used in the body. If ``m`` is a ``maybe``
value, for example, we might have a pattern match that looks like this:

.. code-block:: DaeDaLus

    case m of
      just x  -> ... something using x ...
      nothing -> ...

Finally, there is a special pattern, ``_``, which can be used as a final
"catch-all" case when you don't care what is matched. This should always be
used as the last pattern, or else anything below it will never run!

.. warning::

    Remember: Patterns are checked in order, so be careful to consider that
    when writing complex pattern-matches with ``case``!

    Additionally, if your patterns don't cover all possibilities, note that
    failure and backtracking will occur for the uncovered cases. Some languages
    make sure all patterns are covered, but DaeDaLus isn't one of them!

Loops
^^^^^

``for ...``
"""""""""""

To visit all elements of an array or dictionary, DaeDaLus provides an unusual
form of the familiar ``for`` loop.

It's best to demonstrate this with an example, taken from the PPM spec:

.. code-block:: DaeDaLus

    def Natural = {
      @ds = Many (1..) Digit;
      ^ for (val = 0; d in ds) (addDigit val d);
    }

In the ``for`` loop, we declare a variable ``val`` which acts as an accumulator
value - the value of the body of the loop is what this variable is updated to
after each iteration, and the final value of the entire loop is the final value
of this variable.

Following this declaration is ``d in ds``, which introduces a variable ``d`` to
take on each value in the collection. If ``ds`` is the array ``[1, 2, 3]``,
during the first iteration ``d`` will be 1, during the second it will be 2, and
so on.

Finally, after these declarations, is the loop body. As stated: The value of
this body is what the variable ``val`` takes on each iteration.

Let's break down the PPM example to make this behavior crystal clear. Note that
the function ``addDigit val d`` computes ``10 * val + d``, which is a common
pattern to accumulate parsed digits into a single numerical value.

Let's say for the sake of example that ``ds = [1, 2, 3]``. During the first
iteration, ``val = 0`` and ``d = 1``. So, after this iteration, we can think of
evaluation as proceeding by computing the value of this new loop:

.. code-block:: DaeDaLus

    for (val = 1; d in [2, 3]) (addDigit val d)

``val`` is 1 since the previous iteration's body computed ``addDigit 0 1``.
The body of this new loop is, then, ``addDigit 1 2 = 10 * 1 + 2 = 12``.
So, moving to the next iteration, we have:

.. code-block:: DaeDaLus

    for (val = 12; d in [3]) (addDigit val d)

Which, continuing in the same way, gives a body of
``addDigit 12 3 = 12 * 10 + 3 = 123``. So finally we have:

.. code-block:: DaeDaLus

    for (val = 123; d in []) (addDigit val d)

Since the list is empty, the body is not evaluated again, as there's nothing to
bind ``d`` to. So, we're done! We return this final value of ``val``, namely
``123``.

If you also need access to the index (or key if iterating over a dictionary),
you can use this form:

.. code-block:: DaeDaLus

    for (val = 0; i,x in xs) ...

``map ...``
"""""""""""

If rather than accumulating you wish to *transform* a sequence of elements, you
can use the ``map`` construct. It is syntactically similar to ``for``, except
no accumulation variable is bound:

.. code-block:: DaeDaLus

    map (x in xs) ...

The returned collection is of equal size to that being mapped over, and each
element is given by the value of the body for the corresponding element in the
original collection.

As with ``for``, you can also bind a variable to the index/key:

.. code-block:: DaeDaLus

    map (i,x in xs) ...

More on Types
-------------

Annotating Types
^^^^^^^^^^^^^^^^

So far, we've only mentioned types at all in the context of alternatives
parsers - specifically, to be able to define two parsers that return exactly
the same type of data, overriding the default behavior of introducing a new
type with the same name as the parser. In that case, we used a *type
annotation* to specify the type an expression should have.

We can in fact annotate types in this way anywhere we have an expression - in
general, we write ``e : t`` to mean that expression ``e`` should have type
``t``.

.. note::

    We recommend that all top-level declarations have type annotations, as types
    can act as an excellent form of documentation in addition to comments. Other
    type annotations can be used, but are (mostly) unnecessary due to type
    inference.

    Note that there is a significant difference between these two declarations:

    .. code-block:: DaeDaLus

      def x = 1 : uint 8
      def y : uint 8 = 1

    The first declaration assigns the value of the annotated expression
    ``1 : uint 8`` the name ``x``, the second says that the name ``y``
    ought to have the type ``uint 8`` - in other words, this latter form is
    what we mean by annotating the type of a top-level declaration.

    If a declaration has parameters, they may have their types annotated - in
    this case, we surround the parameter and its type with parentheses, like
    so:

    .. code-block:: DaeDaLus

      def P (Q : uint 8) = R

Unknown Types
^^^^^^^^^^^^^

We can also name types without being explicit about what they are. We write
*type variables* with a ``?`` followed by a name, which is typically a
lowercase letter. For example: The type ``maybe ?a`` can be used to annotate
an expression for which we want the type to be ``maybe`` of *something*.

.. warning::

  Unfortunately, in DaeDaLus, the naming scheme described for unknown types is
  reused for another unrelated feature: implicit parameters. We'll say more
  about implicit parameters when discussing the standard library in a later
  section.

Type Coercion
^^^^^^^^^^^^^

As you'll often find, it is useful to be able to convert a semantic value of
some type (usually numerical) into a semantic value of a different type. This
will be particularly crucial to developing a solid understanding of the
``bitdata`` construction, which will be explained as part of the extended
exercise that follows.

DaeDaLus provides three ways to coerce one type into another:

* ``e as T`` checks statically that the type ``T`` has enough bits to
  losslessly represent the value of ``e``, and performs the conversion if this
  is the case (failing at compile-time otherwise)
* ``e as! T`` always succeeds, but it is *not* lossless: if the original value
  fits in the size of ``T``, the behavior is the same as ``e as T``; otherwise,
  behavior is implementation dependent
* ``e as? T`` performs a *run-time* check that the conversion doesn't lose
  information; if that check succeeds, the behavior is the same as ``e as T``.
  Otherwise, coercion fails, and backtracking occurs

.. note::

  Pay attention to that final description! From it, we can deduce that
  ``e as? T`` is *not* an expression like the other two coercion forms - it is
  a parser, since it can fail and trigger backtracking.

With this, we've covered all of the essential types of values and control-flow
structures. There are a few others for more specialized use-cases; you can check
out the :ref:`control structures` section of the main user guide for details on
how to use these features.

What follows is a "tutorial by immersion" - you'll implement a much more
complicated layout specification one step at a time, with any gaps in necessary
knowledge filled in. The exercises are followed by solutions, so if you get
stuck, you can skip ahead to those solutions for clarification.
