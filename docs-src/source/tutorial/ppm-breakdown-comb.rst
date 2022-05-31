Breaking down PPM: Parser Combinators
=====================================

The primitives are critical to defining parsers, but aren't very interesting
on their own - we need ways to sequence them, represent notions of choice,
and deal with repetition. First, we look at the various ways to sequence a
collection of parsers.

Standard Sequencing
-------------------

First and foremost, we need a way to run one parser after another. In DaeDaLus,
we write sequenced parsers by surrounding them with curly braces (``{ ... }``)
and separating the parsers with semicolons. We've already seen this in the
``Token`` parser:

.. code-block:: DaeDaLus

    def Token P = {
      $$ = P;
      Many (1..) WS;
    }

This says "run the parser ``P``, then run the parser ``Many (1..) WS``." If
either of these were to fail, the entire sequence would fail.

By default, when we use ``{ ... }`` for parser sequencing, the result of the
last parser in the sequence is what will be returned. We can subvert this
default, as in the ``Token`` example, using the special variable ``$$``:
Assigning to this variable in a sequence means "return this as the result
of the whole sequence" - as we'll see later, this is simply *syntactic sugar*
for a more verbose construction with exactly the same behavior.

.. warning::
    There is another way of writing sequenced parsers in DaeDaLus that you may
    see sometimes that relies on whitespace-sensitivity / code layout, in much
    the same way as the Python programming language.

    Consider the two following declarations:

    .. code-block:: DaeDaLus

        def UseBraces = { Match "A"; Match "B" }
        def UseLayout =
          block
            Match "A"
            Match "B"

    We can use layout instead of braces/semicolons using the ``block`` keyword.
    All of the parsers we are sequencing must be aligned on the same column,
    and any text that is indented beyond this column belongs to the
    corresponding (preceding) parser. To demonstrate:

    .. code-block:: DaeDaLus

        def UseLayout2 =
          block
            Match
              "A"
            Match "B"

    Which behaves the same as both of the previous parsers. Neither style is
    preferred by the language; use what's comfortable for you!

Array Sequencing
----------------

We may also use square braces (``[ .. ]``) for sequencing parsers. When we use
this notation, rather than returning a single result from one of the sequenced
parsers, we return an array containing *all* of the results. Crucially, in this
case, all of the parsers being sequenced must return the same type of semantic
value, since array elements must all have the same type.

.. warning::
    Remember: Arrays in DaeDaLus must contain only elements of the same type!
    If you need to package up data of varying types, keep reading on about
    structure sequencing.

Structure Sequencing
--------------------

What if we need to keep the results of multiple parsers, but they return
different types of semantic values? Neither standard nor array sequencing are
sufficient, so we need something a bit fancier.

In many programming languages, we can define *record types* that store a
collection of named fields, each of which has its own type. For example,
a ``Person`` record might contain a field ``name`` of type ``string``, and an
``age`` field of type ``uint 8``. We build a new ``Person`` by providing values
for each field, and from a ``Person`` we may extract the values of each field,
typically using some kind of "field access" notation.

DaeDaLus also supports record types, though in a non-traditional way: A record
is defined by a corresponding parser. This idea is best shown by example.

In the PPM specification, we have the following declaration for a parser
(pop quiz: how do we know it's a parser?) called ``RGB``:

.. code-block:: DaeDaLus

    def RGB = {
      red   = Token Natural;
      green = Token Natural;
      blue  = Token Natural;
    }

Note here that, rather than simply sequencing three parsers, we are storing the
result of each in a variable. Doing so in this way means that the semantic
value produced by the ``RGB`` parser will be a record (hereafter referred to as
a *structure*) with three fields, ``red``, ``green``, and ``blue``, and with a
type named after the parser itself, i.e. ``RGB``. As you might hope, the names
we introduce are available to be referred to later in the sequence of parsers,
so if we needed to, we could use the value stored in ``red`` while parsing
``green`` or ``blue``.

To better demonstrate this last point, consider this more contrived example:

.. code-block:: DaeDaLus

    def S =
      block
        x = UInt8
        y = ^ x + 17

This defines a parser named ``S`` which will return a semantic value that is a
structure (whose type is also named ``S``) with two fields, ``x`` and ``y``,
where ``x`` is a byte we parse and ``y`` is that byte plus 17.

.. note::

    It is also possible to define *local variables* within a declaration
    without causing a structure to be created - this can be useful when we want
    to save parsing results for later, or have some complex semantic value that
    we don't want to write down more than once.

    To introduce a local variable that won't be turned into a structure field,
    prefix the assignment with the keyword ``let`` (or the symbol ``@``). We've
    already seen an example of this in the ``Digit`` parser:

    .. code-block::

        def Digit = {
          @d = Match1 ('0' .. '9');
          ^ d - '0';
        }

    Here, the result of the parser ``Match1 ('0' .. '9')`` is stored in a local
    variable ``d``, which we later use in a lifted semantic value to return the
    value of the digit itself.

    Remember: If we prefix the assignment with ``let`` or ``@``, we're *just*
    creating a local variable, *not* the field of a structure!

De-Sugaring Nonstandard Structure Sequences
-------------------------------------------

Let's pull back the curtain a bit: As it turns out, most of the constructs
for sequencing we've looked at so far can be expressed using only local
variables and standard sequencing!

First, recall that the special variable ``$$`` allows us to control which
parser's result is returned in a standard sequence - if we have
``{ $$ = P; Q }``, that means "run parser ``P``", then run parser ``Q``,
and return the result of parser ``P``." Can we write this without using the
special variable?

Yes! All we need to do is store the result of ``P`` to refer to later, like
so: ``{ @x = P; Q; ^ x }``. Here, we store the result of ``P`` in the local
variable ``x``, which we later lift using the primitive pure parser ``^``.

Similarly, array sequencing of parsers, such as ``[ P; Q ]``, can be
written: ``{ @x0 = P; @x1 = Q; ^ [x0, x1] }``. Note that, in both this and
the previous case, the expanded forms require us to come up with more names
for things. Arguably, naming is one of the hardest problems we face in
computer science, so it's nice to be able to avoid coming up with new names
using the shorthand originally presented.

Finally, even structure sequencing can be written this way, since we can
construct structure semantic values using the primitive pure parser. If
we have ``{ x = P; y = Q }``, this can also be written
``{ @x = P; @y = Q; ^ { x = x, y = y } }``.

While we recommend using the shorthand, developing an understanding of what
it actually means can make it more obvious when each construct is
appropriate for your use-cases.

Parsing Alternates
------------------

While it is great to be able to parse many things in sequence, most interesting
formats require that we be able to parse one of a set of *alternatives* - as an
example, in a programming language, there are typically many different forms of
expression, and anywhere an expression is allowed, we must be able to
successfully parse any of those different forms.

DaeDaLus is unique in that it provides two ways of handling alternatives:
*biased choice* and *unbiased choice* - many parsing libraries do not provide
this flexibility. We'll now look at these alternatives (no pun intended), and
some examples that demonstrate their differing behaviors.

Note that our working PPM example does not use any alternative parsing - the
extended exercise following this section, to implement the PNG image format,
will show off these features more concretely.

Biased Choice Parsing
^^^^^^^^^^^^^^^^^^^^^

If we have two parsers, ``P`` and ``Q``, we can construct the parser
``P <| Q``. This new parser succeeds if either ``P`` or ``Q`` succeeds, and
crucially, when *both* succeed, it behaves like ``P`` (the symbol should
you of this.) Thought about another way: ``P <| Q`` tries to parse using
``P``, and if this fails, it backtracks and tries parsing with ``Q``.

Consider this contrived example:

.. code-block:: DaeDaLus

    def P = (Match1 'A') <| (^ 'B')

``P`` consumes a single byte, ``'A'``, and returns it, or it consumes nothing
and returns the byte ``'B'`` (in the case that parsing a single ``'A'`` fails.)
Important to note is that, on inputs starting with ``'A'``, ``P``'s behavior is
unambiguous - it will always consume the ``'A'``, rather than consuming
nothing.

Unbiased Choice Parsing
^^^^^^^^^^^^^^^^^^^^^^^

We can also construct the parser ``P | Q`` from two parsers ``P`` and ``Q``.
Like biased choice, this parser succeeds if either ``P`` or ``Q`` succeed -
However, when *both* succeed, it is *ambiguous*, and can parse inputs in more
than one way. Typically, these ambiguities are handled by sequencing with other
parsers.

If we take our biased choice example and replace ``<|`` with ``|``:

.. code-block:: DaeDaLus

    def P = (Match1 'A') | (^ 'B')

``P`` is now ambiguous on inputs that start with ``'A'``, since it can consume
either one or zero bytes - remember, DaeDaLus parsers in general only need to
match a prefix of the input to succeed.

There are many grammars that have intentional ambiguities, and this unbiased
choice facility in DaeDaLus allows us to express those formats with ease.

.. note::

    Much like with parser sequencing, we can use a layout-based syntax to write
    down alternatives parsers. We use the keyword ``First`` for biased choice,
    and ``Choose`` for unbiased choice, like so:

    .. code-block:: DaeDaLus

        def BP =
          First
            block
              Match "This is"
              Match "the first alternative"
            Match
              "The second one is here"

        def UP =
          Choose
            block
              Match "This is"
              Match "the first alternative"
            Match
              "The second one is here"

    Again, the language does not prefer this style over the use of ``<|`` and
    ``|`` - use whatever syntax is more comfortable for you. There is one major
    exception to this, which we'll address in the next section.

Tagged Sum Types
^^^^^^^^^^^^^^^^

Something not mentioned above is that, like array-sequenced parsers,
alternative parsers must parse to the same type of semantic value on all
branches - but this is limiting! What if, for example, we're parsing a format
that allows strings or numbers to appear in the same place? As described so
far, we can't handle this using biased or unbiased choice.

Enter *sum types*.

In many programming languages, sum types are how we can describe a set of
alternatives. They are a 'dual' to record types, which are also known as
*product* types. Typically, the *variants* of a sum type are labeled with a
*tag*, which may or may not carry some additional data of some other type.

As a simple example, we can think of the type ``bool`` as a sum type with
two variants, both of which are simply tags: ``true`` and ``false``.

DaeDaLus allows us to return a tagged sum type using variations of the
layout-based syntax described in the note above, similar to how we can
build structures using parser sequencing. Note that we can't use the infix
operators ``<|`` or ``|`` to accomplish this same goal - we *must* use
``First`` and ``Choose``.

As usual, this concept is best demonstrated by an example:

.. code-block:: DaeDaLus

    def GoodOrBad =
      First
        good = Match1 'G'
        bad  = Match1 'B'

This parser returns a semantic value of a new tagged sum type named
``GoodOrBad``, which has two variants whose tags are ``good`` and ``bad``; this
is like parser sequencing that produces structures, except rather than each
variable corresponding to a field, each corresponds to one of the variants of
the sum type.

.. note::

    You might wonder if, like sequencing earlier, there is some syntactic sugar
    at play. Indeed, we can construct semantic values of tagged-sum types
    explicitly, using a special "barbed wire" bracket:

    .. code-block:: DaeDaLus

        def GoodOrBad2 =
          First
            block
              @x = Match1 'G'
              ^ {| good = x |}
            block
              @x = Match1 'B'
              ^ {| bad = x |}

    Note that, because of the way DaeDaLus attempts to infer the types of these
    sum-typed values, this declaration will in fact create a *new* sum type
    named ``GoodOrBad2`` - it is *not* interchangeable with the previous
    definition of ``GoodOrBad``, even though both types have essentially the
    same values.

    If we wanted this new parser to return the same type of semantic value as
    the original ``GoodOrBad``, we would need to provide *type annotations* to
    guide the type inferencer:

    .. code-block:: DaeDaLus

        def GoodOrBad3 =
          First
            block
              @x = Match1 'G'
              ^ {| good = x |} : GoodOrBad
            block
              @x = Match1 'B'
              ^ {| bad = x |}

    Note that, since all branches of ``First`` and ``Choose`` parsers must have
    the same type, we need only annotate the first branch's result - the type
    inferencer will take care of the rest.

With this set of rich type-constructing mechanisms, you can go forth and create
many interesting format specifications with DaeDaLus - but, what happens when
you need to parse many copies of the same thing in sequence, perhaps an unknown
number of times?

Repeating Parsers
-----------------

If we need to parse the same thing multiple times, we can use the ``Many``
parser combinator. In its most basic form, it can parse an arbitrarily long
sequence, stopping only when the given parser first fails. As a simple example:

.. code-block:: DaeDaLus

    { $$ = Many (Match1 '7'); Match1 '0' }

This parser will match any number of 7s followed by a 0, e.g.
``"0"``, ``"70"``, ``"770"``, etc. The semantic value returned by the above
parser is an array of all the 7s that were parsed.

Be cautious when using this unbounded form of ``Many``! It parses inputs
maximally, so it's possible to accidentally create a parser that never
succeeds, e.g.:

.. code-block:: DaeDaLus

    { Many (Match1 '7'); Match1 '7' }

When we know that we are only parsing a particular number of things (or even
that there is a lower or upper bound on the number of things), we can provide
an optional additional argument to ``Many``:

* The parser ``Many n P`` succeeds if ``P`` succeeds exactly ``n`` times
* The parser ``Many (i .. j) P`` succeeds if ``P`` succeeds at least ``i`` and
  at most ``j`` times

This latter form can be modified to leave off either the lower or upper bound,
e.g. ``Many (i ..) P``, which will succeed if ``P`` succeeds at least ``i``
times.

All we're missing now for a complete understanding of the PPM example is some
control-flow mechanisms and expressions involving semantic values - if you're
already familiar with other programming languages, you can probably figure out
what's going on with the ``for``-loops and integer expressions, but the
following section will explain these features in more detail.
