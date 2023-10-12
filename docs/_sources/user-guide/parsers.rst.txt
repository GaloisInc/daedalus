Parsers
=======


Primitive Parsers
-----------------

**Specific Byte.** The parser ``$[ set ]`` matches a single byte that
belongs to the set of bytes described by *set*.
For example, ``$[ '0' .. '9' ]`` matches any bytes in the range 48 (``'0'``)
through 57 (``'9'``) inclusive.  See :ref:`character_classes` for deatails
on how to write sets of bytes.  

**Any Byte.** The parser ``UInt8`` extracts a single byte from the input.
It fails if there are no bytes left in the input.  If successful, it constructs
a value of type ``uint 8``.  This is equivalent to writing ``$[ $any ]``.

**Specific Byte Sequence.** The parser ``Match bytes`` matches the byte
sequence *bytes* in the current input. The resulting semantic value is an
array of bytes, ``[uint 8]``, corresponding to the matched bytes.
For example ``Match "keyword"`` will match ``"keyword"``, while
``Match [0x00,0x01]`` will match two bytes: 0 followed by 1.

**End of Input.** The parser ``END`` succeeds only if there is no more input
to be parsed.  If successful, the result is the trivial semantic value ``{}``.
Normally DaeDaLus parsers succeed as long as they match a *prefix* of the
entire input.  By sequencing (see `Sequencing Parsers`_) a parser with
``END`` we specify that the entire input must be matched.

**Pure Parsers.** Any semantic value may be turned into a parser that does
not consume any input and always succeeds with the given result.  To do
so prefix the semantic value with the operator ``^``.  Thus, ``^ 'A'`` is
a parser that always succeeds and produces byte ``'A'`` as a result.
The parser ``Accept`` may be used to match the empty string without
constructing an interesting semantic value.  It is equivalent to ``^ {}``.
In many cases, one may omit the ``^`` operator because of :ref:`implicit_lifting`.


**Explicit Failure** The ``Fail`` construct will always fail.  This
parser is parameterized by an optional location, along with an error
message.

**Examples:**

.. code-block:: DaeDaLus

  {- Declaration                      Matches       Result          -}
  def GetByte     = UInt8             -- Any byte X X
  def TheLetterA  = $[ 'A' ]          -- Byte 65    65
  def TheNumber3  = $[0 .. 5]         -- A byte between 0 to 5
  def TheNumber16 = $[ 0x10 ]         -- Byte 16    16
  def Magic       = Match "HELLO"     -- "HELLO"    [72,69,76,76,79]
  def AlwaysA     = ^ 'A'             -- ""         65
  def GiveUp      = Fail "I give up"  -- (none)    Failure with message "I give up"

Sequencing Parsers
------------------

Basic Sequencing
^^^^^^^^^^^^^^^^

Multiple parsers may be executed one after the other,
by listing them either between ``{`` and ``}`` or between ``[`` and ``]``,
and separating them with ``;``.  Thus, ``{ P; Q; R }`` and ``[ P; Q; R ]`` are
both composite parsers that will execute ``P`` , then ``Q``, and finally ``R``.
If any of the sequenced parsers fails, then the whole sequence fails.

Parsers sequenced with ``[]`` produce an array, with each element of the
array containing the result of the corresponding parser.
Since all elements in an array have the same type, all parsers sequenced
with ``[]`` should construct the same type of semantic value.

By default, parsers sequenced with ``{}`` return the result of the last
parser in the sequence.

Examples:

.. code-block:: DaeDaLus

  {- Declaration                                  Matches       Result -}
  def ABC1 = { $['A']; $['B']; $['C'] }        -- "ABC"         67
  def ABC2 = [ $['A']; $['B']; $['C'] ]        -- "ABC"         [65,66,67]
  def ABC3 = { Match "Hello"; Match "ABC" }    -- "HelloABC"    [65,66,67]
  def ABC4 = { Match "Hello"; $['C'] }         -- "HelloC"      67

An alternative notation for ``{ .. }`` parsers is to use the ``block`` keyword
and *layout*:

.. code-block:: DaeDaLus

  def UseBraces = { Match "A"; Match "B" }
  def UseLayout =
    block
      Match "A"
      Match "B"

The parsers ``UseBraces`` and ``UseLayout`` are the same, just using a
different notation.  When using layout, the entries in the sequence must start
on the same column, and any text that is indented more than that column
belongs to the corresponding parser.  So, the following parser is also
equivalent to the previous two:

.. code-block:: DaeDaLus

  def AlsoTheSame =
    block
      Match
        "A"
      Match "B"



Explicit Result
^^^^^^^^^^^^^^^

A ``block`` or ``{}``-sequenced group of parsers may
return the result from any member of the group instead of the last one.
To do so, assign the result of the parser to the special variable ``$$``.
For example:

.. code-block:: DaeDaLus

  def ReturnMiddle =
    block
      P
      $$ = Q
      R

In the example above, the semantic value produce by ``ReturnMiddle`` is that
produced by ``Q``.


Local Variables
^^^^^^^^^^^^^^^

It is also possible to combine the results of some
of the ``block/{}``-sequenced parsers by using *local variables* and the
pure parser.  Assignments prefixed by the keyword ``let`` introduce a local
variable, which is in scope in the following parsers.  Here is an example:

.. code-block:: DaeDaLus

  def Add =
    block
      let x = UInt8
      $[ '+' ]
      let y = UInt8
      ^ x + y

The parser ``Add`` is a sequence of 4 parsers.  The local variables ``x``
and ``y`` store the results of the first and the third parser.  The result
of the sequence is the result of the last parser, which does not consume
any input, but only constructs a semantic value by adding ``x`` and ``y``
together.


Structure Sequence
^^^^^^^^^^^^^^^^^^

It is also possible to return results from more than
one of the parsers in a ``block/{}``-sequenced group.  To do so give names
to the desired results (*without* ``let``).  The semantic value of the
resulting parser is a structure with fields containing the value of
the correspondingly named parsers.  Consider, for example, the
following declaration:

.. code-block:: DaeDaLus

  def S =
    block
      x = UInt8
      y = Match "HELLO"

This declaration defines a parser named ``S``, which will extract a
byte followed by the sequence ``"HELLO"``. The result of this parser is
a *structure type*, also named ``S``, which has two fields, ``x`` and ``y``:
``x`` is a byte, while ``y`` is an array of bytes.

Note that structure fields also introduce a local variable with the same name,
so later parsers in the sequence may depend on the semantic values in
earlier parsers in the sequence.  For example:

.. code-block:: DaeDaLus

  def S1 =
    block
      x = UInt8
      y = block
            let z = UInt8
            ^ x + z

The parser ``S1`` is a sequence of two parsers, whose semantic value
is a structure with two fields, ``x`` and ``y``.  Both fields have type
``uint 8``.  The first parser just extracts a byte from input.  The second
parser is itself a sequence: first it extracts a byte from the input,
but its semantic value is the sum of the two extracted bytes.  As another
example, here is an equivalent way to define the same parser:

.. code-block:: DaeDaLus

  def S2 =
    block
      x = UInt8
      let z = UInt8
      y = ^ x + z


Syntactic Sugar
^^^^^^^^^^^^^^^

A number of the constructs described in this section are
may be thought of as simply syntactic sugar for using local variables.
Here are some examples:

+----------------------+-----------------------------------------------------+
| Expression:          |  Equivalent to:                                     |
+======================+=====================================================+
| ``{ $$ = P; Q }``    | ``{ let x  = P; Q;          ^ x }``                 |
+----------------------+-----------------------------------------------------+
| ``[ P; Q ]``         | ``{ let x0 = P; let x1 = Q; ^ [x0,x1] }``           |
+----------------------+-----------------------------------------------------+
| ``{ x = P; y = Q }`` | ``{ let x  = P; let y  = Q; ^ { x = x; y = y } }``  |
+----------------------+-----------------------------------------------------+


Parsing Alternatives
--------------------

Biased Choice
^^^^^^^^^^^^^
Given two parsers ``P`` and ``Q`` we may construct the composite
parser ``P <| Q``.   This parser succeeds if *either* ``P`` *or* ``Q``
succeeds. In the case that *both* succeed, the parser behaves like ``P``.
Note that ``P`` and ``Q`` have to construct semantic values of the same type.

More operationally, ``P`` would be used to parse the input first,
and only if it fails would we execute ``Q`` on the same input.  While this
may be a useful intuition about the behavior of this parser, the actual
parsing algorithm might implement this behavior in a different way.

Here are some examples:

.. code-block:: DaeDaLus

  {- Declaration           Matches        Result   -}
  def B1 = $[ 'A' ]     -- "A"            'A', or
        <| $[ 'B' ]     -- "B"            'B'

  def B2 = $[ 'A' ]
        <| ^ 'B'        -- "A"            'A', or
                        -- ""             'B'

These two are quite different:
  * ``B1`` matches a single byte, either ``A`` or ``B`` and returns the
    matched byte as the result of the parser.
  * ``B2`` matches either 1 byte, which must be ``A`` and will be returned
    as the result of the parser, or 0 bytes, in which case it will return
    byte ``B``.


Unbiased Choice
^^^^^^^^^^^^^^^

Given two parsers ``P`` and ``Q`` we may construct the composite
parser``P | Q``.  This parser succeeds if either ``P`` or ``Q``
succeeds on the given input.   Unlike biased choice, if *both* succeed,
then the resulting parser is *ambiguous* for the given input, which means
that input may be parsed in more than one way.  It is possible, however, to
resolve ambiguities by composing (e.g., in sequence) with other parsers.

Here are some examples:

.. code-block:: DaeDaLus

  def U1 = $[ 'A' ] | ^ 0
  def U2 = { U1; 'B' }

Parser ``U1`` on its own is ambiguous on inputs starting with ``"A"`` because
it could produce either ``'A`` (by consuming it from the input),
or ``0`` (by consuming nothing).  This happens because parsers only need
to match a prefix of the input to succeed.

Parser ``U2`` accepts inputs starting with either ``"AB"`` (by using the
left alternative of ``U1``) or starting with ``"B"`` (by using the right
alternative of ``U1``).  No inputs are ambiguous in this case.


Alternative Syntax
^^^^^^^^^^^^^^^^^^

Given multiple parsers ``A``, ``B``, ... we can use the ``Choose`` keyword
for unbiased choice and ``First`` for biased choice.  These constructs
use layout, in a similar style to ``block``:  when using this notation
eahc alternative must start at the same indention in the file, and the
entire definition of an alternative must be indented furter.  Here are
some examples:

.. code-block:: DaeDaLus

  def BiasedExample =
    First
      block
        Match "This is"
        Match "the firts alternaitve"
      Match
        "The second one is here"

  def BiasedExample =
    Choose
      block
        Match "This is"
        Match "the firts alternaitve"
      Match
        "The second one is here"


Tagged Unions
^^^^^^^^^^^^^

DaeDaLus supports a variation on ``Choose`` and ``First``
that can be used to construct tagged unions, which is useful if
you'd like the semantic value to reflect which of the parsers succeeded,
or if the branches need to return construct results of different types.

For example, the following parser constructs a union with possible tags
``good`` and ``bad``, depending on whether the input character is
``'G'`` or ``'B'``. 

.. code-block:: DaeDaLus 

  def BorG =
    First
      good = $[ 'G' ]
      bad  = $[ 'B' ]

This parser works in a similar way to ordinary ``First`` except that if
an alternative succeeds, the resulting semantic value is *tagged* with
the given tag (e.g., ``good`` or ``bad`` and the previous example).  The type
of the semantic value is of a new user-defined type, derived from the name
of the declaration---in the previous example, the result of the parser would
of a newly defined union type called ``BorG``.


It is also possible to construct a value if a tagged-union type using
the notation ``{| good = 'G' |}``.  For example, an alternative way
to write the previous example is like this:

.. code-block:: DaeDaLus

  def AnotherBorG =
    First
      block
        let x = $[ 'G' ]
        ^ {| good = x |}
      block
        let x = $[ 'B' ]
        ^ {| bad = x |}

Note that when using the ``{| tag = value |}`` notation, DaeDaLus will try
to infer the type of the tagged union.  If it cannot infer it, it will generate
a new user defined type:  this is the case in the previous example, and so
parser ``AnotherBorG`` will return values of a newly generated type also
called ``AnotherBorG``.

It is important to note that even though ``BorG`` and ``AnotherBorG`` have
essentially the same values, these values have distinct types and **cannot**
be freely interchanged.

If we want to make a tagged union value of an existing type, we'd have to
provide a *type annotation*, unless the type can already be inferred from
the context.   For example:

.. code-block:: DaeDaLus

  def YetAnotherBorG =
    Choose
      block
        let x = $[ 'G' ]
        ^ {| good = x |} : BorG
      block
        let x = $[ 'B' ]
        ^ {| bad = x |}

The ``: BorG`` in the first alternative specifies that we are making a value
of type ``BorG``.  Note that we do not need to provide the annotation on the
second alternative because all alternatives in (untagged) ``Choose`` have
the same type, so DaeDaLus can infer that we are also making a value of
type ``BorG``.

Unions can also be declared explicitly for use in the above scenarios
rather than declaring them implicitly by using ``Choose`` or ``First``.
For example:

.. code-block:: DaeDaLus

  def MyUnion =
    union
      Good: uint 8
      Bad: [uint 8]

In this example, we have explicitly declared a union ``MyUnion`` with
two constructors, ``Good`` and ``Bad``. The ``Good`` constructor carries
a single ``uint 8`` and the ``Bad`` constructor carries a list of ``uint
8``. Such a union could then be used in a parser as follows:

.. code-block:: DaeDaLus

  def MyUnionParser: MyUnion =
    Choose
      block
        let x = UInt8
        {| Good = x |}
      block
        {| Bad = "Some text" |}

To use an explicitly-declared union, we give ``MyUnionParser`` a return
type annotation to indicate that it returns values of type ``MyUnion``.
We then construct union semantic values using the ``Good`` and ``Bad``
constructors and the ``{| ... |}`` notation.

Repetition
----------

The ``Many`` construct allows the same parser to be run multiple times
in sequence on an incoming data stream, and it returns an array containing
the resulting semantic values.

.. code-block:: DaeDaLus

  block
    $$ = Many $[ '7' ]
    $[ '0' ]
    END

This code will successfully parse any stream consisting of multiple ``7``
characters, terminated by the ``0`` character at the end of the stream. For
example, the stream ``"7770"`` will return the array ``['7', '7', '7']``. 

The ``Many`` construct optionally takes either a ``uint 64`` value or an
interval bounded by two ``uint 64`` values: 

* ``Many n P`` succeeds if it executes parser ``P`` exactly ``n`` times.

* ``Many (i..j) P`` succeeds if it executes parser ``P`` at least ``i`` and
  at most ``j`` times. 

* ``Many`` also supports lower-bounded intervals ``Many (i..) P``, and
  likewise upper-bounded intervals ``Many (..j) P``.

To avoid spurious backtracking, ``Many`` will parse any input maximally.
This can have counter-intuitive consequences! For example, the following
code will never succeed: 

.. code-block:: DaeDaLus 

  block
    Many $[ '7' ]
    $[ '7' ]

The call to ``Many`` will consume all the input characters matching ``7``,
meaning that the following ``$[ '7' ]`` will always fail. This may be difficult
to spot in situations where two more complex parsers are run in sequence,
the first of which contains an unbounded call to ``Many``.


