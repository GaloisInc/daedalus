Breaking down PPM: Parser Combinators
=====================================

These primitives are critical to defining parsers, but aren't very interesting
on their own - we need ways to sequence them, represent notions of choice,
and deal with repetition. First, we look at the various ways to sequence a
collection of parsers.

Standard Sequencing
-------------------

First and foremost, we need a way to run one parser after another. In DaeDaLus,
we write sequenced parsers by surrounding them with curly braces (``{ ... }``)
and separating the parsers with semicolons. We've already seen this in the
``Token`` parser above:

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

.. note::

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
