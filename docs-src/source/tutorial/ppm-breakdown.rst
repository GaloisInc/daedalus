Breaking Down the PPM Specification
===================================

In :ref:`getting started`, we presented this DaeDaLus specification for the PPM
ASCII image format:

.. literalinclude:: examples/plain-ppm.ddl
    :language: DaeDaLus

The following sections will break down this specification in order to introduce
the core features of DaeDaLus, including declarations, parsing primitives,
parser combinators, and some of the facilities for constructing semantic values
that match the structure of the data we're writing layout specifications for.
After reading this section, you should be prepared to write all kinds of data
layouts effortlessly.

DaeDaLus Declarations
---------------------

Starting from the very beginning of the PPM specification, we have:

.. code-block:: DaeDaLus

    def Main = {
      $$ = PPM;
    }

This is a *declaration* for the name ``Main``. Declarations in general are
indicated by the keyword ``def``, followed by the name being declared,
any formal parameters, an equals sign, and finally the definition itself.

As in most programming langauges, declarations allow us to name certain
entities that we may wish to refer to later, often with the intent of reducing
duplication and helping readers understand the ideas we're trying to express.
(In the immortal words of Guido Van Rossum, creator of Python: Code is read
more often than it is written.) In this particular case, the name ``Main``
indicates to the DaeDaLus interpreter and backend that this is the entry point
of the parser being defined. All layout specifications must declare a ``Main``
symbol.

.. warning::

    In DaeDaLus, declarations behave differently from what you might be used to
    if you are unfamiliar with pure functional programming. In such languages,
    variables are *immutable*: Once defined, they cannot be re-defined. While
    at first this may seem limiting, it makes it far easier to reason about the
    behavior of code since each name always refers to the same thing - anywhere
    we see the name used, we can substitute its one precise definition without
    changing the meaning of the program or having to be concerned about changes
    to any global state.

Deducing Types From Names
^^^^^^^^^^^^^^^^^^^^^^^^^

Though it's probably not obvious, there's actually another piece of information
we can deduce from this declaration: What type of entity ``Main`` refers to.

In DaeDaLus, there are three sorts of entities that may be declared: *parsers*,
*semantic values*, and *character classes*.

Parsers **always** have a name starting with an uppercase letter; so, in the
example declaration we're currently looking at, ``Main`` is a parser.

In contrast, semantic values **always** have a name starting with a lowercase
letter. Looking ahead in the PPM example, by this convention, the declaration
of ``addDigit`` specifies a semantic value; in particular, a function from two
semantic values to another semantic value.

Finally, character classes **always** have a name starting with ``$``. The PPM
example does not specify any character classes, but we'll have more to say
about them later.

.. note::
    In summary:

    * Parser names begin with uppercase letters
    * Semantic value names begin with lowercase letters
    * Character class names begin with ``$``

    Keeping these rules in mind will save you a lot of trouble debugging in the
    future!

Parameterized Declarations
^^^^^^^^^^^^^^^^^^^^^^^^^^

The next declaration in this specfication shows that a declaration may be
*parameterized*:

.. code-block:: DaeDaLus

    def Token P = {
      $$ = P;
      Many (1..) WS;
    }

Parameter names follow the same rules outlined above: Uppercase names indicate
parser parameters, lowercase names indicate semantic value parameters, and
names starting with ``$`` indicate character class parameters. In this example,
since ``P`` is capitalized, we know it is a parser parameter.

The ``Token`` parser also demonstrates similarities between DaeDaLus and
*parser combinator* libraries such as ``parsec`` for the Haskell programming
language. Rather than having to write complex parsing algorithms from scratch,
a library of primitive parsers and higher-order combining operations are
provided as building blocks. If you're already familiar with these sorts of
parsing libraries, you're well on your way to being a productive DaeDaLus user!

Primitive Parsing
-----------------

Before trying to move on to the other more complex parsers defined in this
specification, it is going to be helpful to outline some of the primitive
parsers made available in the DaeDaLus standard library, pointing to their uses
in the PPM example where appropriate:


``UInt8``
^^^^^^^^^

The parser ``UInt8`` accepts a single byte of input, failing if there are no
bytes left to consume. When it succeeds, it constructs a semantic value of
type ``uint 8``. The PPM example doesn't use this parser, but later we will
see examples of parsers that do.

.. note::
    Use ``UInt8`` when you need to parse a single specific byte from input.

``Match1 ...``
^^^^^^^^^^^^^^

``Match1`` accepts a single byte of input that matches any of the bytes in a
given 'set' of bytes; we'll see ways to write sets of bytes shortly. As an
example to whet your appetite: ``Match1 ('0' .. '9')`` will accept any single
byte between 48 (the ASCII encoding of ``'0'``) and 57, inclusive. In other
words: This parser accepts a single ASCII-encoded digit.

In the PPM example, we can see a number of examples of ``Match1``. One is this
declaration:

.. code-block:: DaeDaLus

    def WS = Match1 (0 | 9 | 12 | 32 | '\n' | '\r')

Here, ``|`` essentially means 'or', allowing us to specify a discrete set of
bytes we consider whitespace in PPM inputs.

.. note::
    Use ``Match1 ...`` when you need to parse one of a finite set of bytes.

``Match ...``
^^^^^^^^^^^^^

``Match`` accepts a particular sequence of bytes in the input, returning a
semantic value that is an array of bytes
(whose type is written ``[uint 8]``.) As an example, we can write
``Match "keyword"`` to match exactly those bytes corresponding to the string
``"keyword"``. We can also utilize this parser when working with binary
formats, where we may find it useful to precisely specify the bytes we're
expecting, e.g. ``Match [0x00, 0x01]`` which will match the two bytes 0 and 1.

In the declaration for the parser ``PPM``, we use ``Match "P"`` to consume the
first part of the aforementioned PPM "magic number"; we could have just as well
used ``Match1 'P'`` here, but the meaning in this case is the same.

.. note::
    Use ``Match`` when you need to parse a sequnce of specific bytes.

``END``
^^^^^^^

The parser ``END`` succeeds only if there is no additional input to be
consumed. It results in the 'trivial' semantic value ``{}``. Typically, a
DaeDaLus parser happily succeeds consuming only a *prefix* of the input, but
adding ``END`` to the end of our parsing sequence means we wish to guarantee
the whole input is consumed.

The PPM specification does not use the ``END`` parser, so in fact the generated
parser will consume any input prefixed by a well-formed ASCII PPM - depending
on the use-case, this may or may not be desirable.

.. note::
    Use ``END`` when you want to guarantee full inputs are consumed.

``^ ...``
^^^^^^^^^

It is sometimes convenient to 'lift' a semantic value into a parser that
consumes no input and always succeeds, returning the given semantic value -
this is an important part of how data-dependent parsing works in DaeDaLus.
We can do so by placing a ``^`` before any semantic value. For example:
``^ 'A'`` is a parser that consumes no input and always succeeds, producing
the byte ``'A'`` as a result. If we want to consume no input *and* return
nothing interesting, we may write ``^ {}`` (the same 'trivial' semantic value
returned by the ``END`` parser) - DaeDaLus also provides the synonym
``Accept`` for this trivially-succeeding parser, for more readability.

The idea here is best shown by example. Consider the declaration of the
``Digit`` parser:

.. code-block:: DaeDaLus

    def Digit = {
      @d = Match1 ('0' .. '9');
      ^ d - '0';
    }

Parsers can only be combined with other parsers - so, to transform the ASCII
byte we read with ``Match1`` into the actual digit it represents, we must
write a parser that returns that transformed value - this is exactly the use
for ``^``, since we don't wish to read any additional input.

Parsers defined with ``^`` are called 'pure', because they do not consume any
input (that is, they don't alter the internal parsing state in any way.) We'll
see many more examples of this in the other examples we study.

.. note::
    Use ``^ ...`` to turn semantic values into parsers that don't consume
    input.

``Fail ...``
^^^^^^^^^^^^

We can trigger a failure with the ``Fail`` parser, which *always* fails.
Optionally, we can provide a message to this parser which will be printed as
part of the triggered failure - this is how you may indicate to users of your
specifications what exactly went wrong while trying to parse.

The PPM example does not make use of the ``Fail`` parser; it is mostly useful
when performing *validation* of parsed data, which is often better left to
later stages of the processing done on layouts. We'll have some more to say
about this later.

.. note::
    Use ``Fail ...`` to immediately stop parsing with an error message.

Sequencing
----------

These primitives are critical to defining parsers, but aren't very interesting
on their own - we need ways to sequence them, represent notions of choice,
and deal with repetition. First, we look at the various ways to sequence a
collection of parsers.

Standard Sequencing
^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^

We may also use square braces (``[ .. ]``) for sequencing parsers. When we use
this notation, rather than returning a single result from one of the sequenced
parsers, we return an array containing *all* of the results. Crucially, in this
case, all of the parsers being sequenced must return the same type of semantic
value, since array elements must all have the same type.

.. warning::
    Remember: Arrays in DaeDaLus must contain only elements of the same type!

Structure Sequencing
^^^^^^^^^^^^^^^^^^^^

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


