*******************
DaeDaLus User Guide
*******************

.. toctree::
   :maxdepth: 2
   :caption: Contents:


DaeDaLus is a domain specific language for specifying parsers.  It supports
data dependent parsing, which allows a parser's behavior to be affected by
the semantic values parsed from other parts of the input.
This allows for a clear, yet precise, specification of many binary formats.

Using the ``daedalus`` Tool
===========================


Help
----

To see a list of options:

.. code-block:: bash

  daedalus --help

Check a Specification
---------------------

To type-check a DaeDaLus specification and see the types of the declarations:

.. code-block:: bash

  daedalus --show-types MyParserSpec.ddl

The resulting types have the following form: 

.. code-block:: DaeDaLus 

  ParserName: 
    parameter: <type A>
    parameter: <type B> 
    ... 
    defines: <type C> 

This resembles a C type declaration as follows: 

.. code-block:: C 

  <type C> ParserName(<type A>, <type B>, ...); 

The types themselves may be simple types such as integers or arrays, but they
often have the form ``parser of <type A>``. This indicates that the parameter or
result is a parser, that itself generates semantic values of type ``A``. 

Run the Interpreter
-------------------

To run interpret a specification on a particular input:

.. code-block:: bash

  daedalus MyParserSpec.ddl --interp=input.txt

If successful, the resulting semantic value will be shown on ``stdout``.
You may also add the flag ``--json`` or ``--html`` to see the semantic value
in the corresponding formats.

.. todo::

  Document the JSON schema


Compile to Haskell
------------------

To compile a DaeDaLus parser specification to Haskell:

.. code-block:: bash

  daedalus MyParserSpec.ddl --compile-hs

The result is a Haskell module which contains definitions for the
parsers and functions defined in the specification.   To use the generated
code you'd have to integrate it with a Haskell project and also use
the ``rts-hs`` package.


Compile to C++
--------------

To compile a DaeDaLus package specification C++ 17:

.. code-block:: bash

  daedalus MyParserSpec.ddl --compile-c++ --out-dir=some_dir_name

This will generate a number of C++ files together with a ``Makefile`` and
a sample driver program in directory ``some_dir_name``.
The ``Makefile`` shows how to build the parser and has an option to
generate Doxygen documentation.




Declarations
============

A DaeDaLus specification consists of a sequence of *declarations*.
Each declaration can specify either a *parser*, a *semantic value*, or
a *character class*.  Parsers may examine and consume input, and have
the ability to fail.  If successful, they produce a semantic value.
Character classes describe sets of bytes, which may be used to define
parsers, and will be discussed in more detail in section `Character Classes`_.

The general form of a declarations is as follows:

.. code-block:: DaeDaLus

  def Name Parameters = Definition

The name of a declaration determines what sort of entity it defines:

  * **parsers** always have names starting with an **uppercase** letter,
  * **semantic values** have names starting with a **lowercase** letter,
  * **character classes** have names starting with the symbol ``$``.

Here are some sample declarations:

.. code-block:: DaeDaLus

  def P   = UInt8       -- a parser named `P`
  def x   = true        -- a semantic value named `x`
  def $d  = '0' .. '9'  -- a character class named `$d`

Single line comments are marked with ``--``, while multi-line comment are
enclosed between ``{-`` and ``-}``, and may be nested.

Declarations may be parameterized, and the parameters of a declaration follow
the same rules.  In a declaration with a parameter ``p``, the lower case
indicates that the parameter is a semantic value, while ``P`` (upper case)
would be a grammar parameter, and ``$p`` a character class parameter.

Consider, for example the following declaration:

.. code-block:: DaeDaLus

  def Example n P $q =
    if n > 0
      then P
      else Match1 $q

This declares a parser called ``Example`` with 3 parameters, ``n``, ``P`` and
``$q``.   Note that the parameters are simply separated by space, and usually
there is no need to provide type annotations as Deadalus can infer the types
based on the naming rules and the uses in the definition.








Parsers
=======


Primitive Parsers
-----------------

**Any Byte.** The parser ``UInt8`` extracts a single byte from the input.
It fails if there are no bytes left in the input.  If successful, it constructs
a value of type ``uint 8``.

**Specific Byte.** The parser ``Match1 $set`` matches a single byte that
belongs to the set of bytes (also referred to as *character class*)
described by ``$set``.

**Specific Byte Sequence.** The parser ``Match bytes`` matches the byte
sequence ``bytes`` in the current input. The resulting semantic value is an
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

**Explicit Failure** The ``Fail`` construct will always fail.  This
parser is parameterized by an optional location, along with an error
message.

**Examples:**

.. code-block:: DaeDaLus

  {- Declaration                      Matches       Result          -}
  def GetByte     = UInt8             -- Any byte X X
  def TheLetterA  = Match1 'A'        -- Byte 65    65
  def TheNumber3  = Match1 (0 .. 5)   -- A byte between 0 to 5
  def TheNumber16 = Match1 0x10       -- Byte 16    16
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

  {- Declaration                                      Matches        Result-}
  def ABC1 = { Match1 'A'; Mathc1 'B'; Match1 'C' }   -- "ABC"       67
  def ABC2 = [ Match1 'A'; Match1 'B'; Match1 'C' ]   -- "ABC"       [65,66,67]
  def ABC3 = { Match "Hello"; Match "ABC" }           -- "HelloABC"  [65,66,67]
  def ABC4 = { Match "Hello"; Match1 'C' }            -- "HelloC"    67

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
      Match1 '+'
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
  def B1 = Match1 'A'   -- "A"            'A', or
        <| Match1 'B'   -- "B"            'B'

  def B2 = Match1 'A'
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
then the resulting parser is *ambigous* for the given input, which means
that input may be parsed in more than one way.  It is possible, however, to
resolve ambiguities by composing (e.g., in sequence) with other parsers.

Here are some examples:

.. code-block:: DaeDaLus

  def U1 = Match1 'A' | ^ 0
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
for unbiased choice and ``Choose1`` for biased choice.

+---------------------------+-------------------+ 
| Expression:               | Equivalent to:    | 
+===========================+===================+ 
| ``Choose { A ; B; ...}``  | ``A | B | ...``   | 
+---------------------------+-------------------+ 
| ``Choose1 { A ; B; ... }``| ``A <| B <| ...`` |
+---------------------------+-------------------+

The ``Choose`` and ``Choose1`` keywords also support **layout**, so instead
of using braces and semi-colons we can just line-up the alternaitves like this:

.. code-block:: DaeDaLus

  def ChooseWithBraces =
    Choose1 {
      Match1 'A';
      Match1 'B';
    }

  def ChooseWithLayout =
    Choose1
      Match 'A'
      Match 'B'


Tagged Unions
^^^^^^^^^^^^^

DaeDaLus supports a variation on ``Choose`` and ``Choose1``
that can be used to construct tagged unions, which is useful if
you'd like the semantic value to reflect which of the parsers succeeded,
or if the branches need to return construct results of different types.

For example, the following parser constructs a union with possible tags
``good`` and ``bad``, depending on whether the input character is
``'G'`` or ``'B'``. 

.. code-block:: DaeDaLus 

  def BorG =
    Choose
      good = Match1 'G'
      bad  = Match1 'B'

This parser works in a similar way to ordinary ``Choose`` except that if
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
    Choose
      block
        let x = Match1 'G'
        ^ {| good = x |}
      block
        let x = Match1 'B'
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
        let x = Match1 'G'
        ^ {| good = x |} : BorG
      block
        let x = Match1 'B'
        ^ {| bad = x |}

The ``: BorG`` in the first alternative specifies that we are making a value
of type ``BorG``.  Note that we do not need to provide the annotation on the
second alternative because all alternatives in (untagged) ``Choose`` have
the same type, so DaeDaLus can infer that we are also making a value of
type ``BorG``.





Repetition
----------

The ``Many`` construct allows the same parser to be run multiple times
in sequence on an incoming data stream, and it returns an array containing
the resulting semantic values.

.. code-block:: DaeDaLus

  block 
    $$ = Many (Match1 '7')
    Match1 '0' 
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
    Many (Match1 '7')
    Match1 '7' 

The call to ``Many`` will consume all the input characters matching ``7``,
meaning that the following ``Match1`` will always fail. This may be difficult
to spot in situations where two more complex parsers are run in sequence,
the first of which contains an unbounded call to ``Many``.


Control Structures 
==================

If-then-else
------------

Booleans may be used to choose between one of two parsers:

.. code-block:: DaeDaLus

  block
    let i = Match1 ('0'..'9')
    if (i - '0') > 5
      then Match 'X'
      else ^ 7

The parser above parses a decimal digit and if it is larger than 5
it will try to match ``'X'`` from the input, otherwise it will succeed
with semantic value 7.



Guards
------

Guards provide one way to examine a semantic value, and their general form is:

.. code-block:: DaeDaLus

  expression is shape

A guard is a parser that will succeed if the expression has the required shape.

Boolean Guards
^^^^^^^^^^^^^^

Perhaps the most common guard is on boolean semantic values,
which may be used to control whether parsing should continue. For example,
the following parser uses the guard ``(i - '0') > 5 is true`` to continue
parsing (on the given alternative) only for digits larger than 5.

.. code-block:: DaeDaLus

  block
    let i = Match1 ('0'..'9')
    Choose1
      block
        (i - '0') > 5 is true
        ^ "input gt 5"
      ^ "input leq 5"

So, if ``p`` is a boolean value, then ``p is true`` is a parser that
succeeds without consuming input if ``p`` holds, and fails otherwise.
Similarly, ``p is false`` is a parser that would succeed only
if ``p`` is ``false``.


Guards on ``maybe``
^^^^^^^^^^^^^^^^^^^

The type ``maybe`` also supports guards, with two shapes:
``just`` and ``nothing``.  For example ``e is just`` is a parser that will
succeed only if ``e`` is of the shape ``just x`` form some ``x``.  In that
case the result of the parser would be the value ``x``.  Guards that have
no interesting result (e.g., ``e is true``) simply return the trivial
value ``{}``.

Guards on Tagged Unions
^^^^^^^^^^^^^^^^^^^^^^^

The same notation may be used to examine values of user-defined
union types (see `Tagged Unions`_)


.. code-block:: DaeDaLus 

  block
    let res = Choose
                good = Match1 'G'
                bad  = Match1 'B'

    Choose

      block
        res is good
        ^ "Success!"

      block
        res is bad
        ^ "Failure!"



Case
----

The ``case`` construct provides an alternative method for examining semantic
values. The body of a case expression consists of a list of matches with the
syntax ``pattern -> result``. For example, the following expression has the same
functionality as the previous example, but avoids the need for backtracking. 

.. code-block:: DaeDaLus 

  block
    let res = Choose
                good = Match1 'G'
                bad  = Match1 'B'
    case res of 
      good -> ^ "Success!"
      bad  -> ^ "Failure!"

A case expression can extract the value from a tagged union. In this case, the 
match should have the form ``pattern var -> result``.

.. code-block:: DaeDaLus 

  block 
    let res = Choose 
                number = Match1 ('0'..'9')
                letter = Match1 ('a'..'z')
                other = Match1 Uint8 
    case res of 
      number n -> ^ (n - '0')
      letter l -> ^ (l - 'a')
      _        -> Fail "Something went wrong" 

Here the special pattern ``_ -> result`` serves as a default, which matches
against any value. Similarly, a pattern of the form ``pattern _ -> result``
indicates that the value will not be used in the result.

In a parser expression, case need not be total (i.e. cover all possible
patterns) as any omitted matches will implicitly result in failure and
backtracking. In non-parser contexts, all case expressions are required to be
total. 

.. todo:: 
  It should be true that guards are just syntactic sugar for case

``for`` loops
-------------

The ``for`` construct can be used to iterate over collections (arrays
and dictionaries).  A for-loop declares a local variable representing
the accumulated result of the computation, and a variable that is
bound to the elements of the collection.  The body may be a parser, or
a semantic value.  For example, the following expression sums the
values in an array of integers:

.. code-block:: DaeDaLus 

  for (val = 0 : int; v in [1,2,3]) 
    val + v

Here, ``val`` is initially bound to ``0``. Each iteration of the loop binds
``v`` to the current element of the sequence, then computes the value of the
body, ``val + v``. This returned value is the updated value of ``val``.

Another way to understand how this works is to see the following expression,
which is the result of one step of evaluation: 

.. code-block:: DaeDaLus 

  for (val = 1; v in [2, 3]) 
    val + v

``for`` supports an alternative form which binds both the index and
value of a collection. For example, the following loop multiplies 
each element in the sequence by its index: 

.. code-block:: DaeDaLus 

  for (val = 0; i,v in [1,2,3]) 
    val + (i * v)  

This construct is also useful when iterating over the contents of
dictionaries, where the index is bound to the key.  The following
loop is a parser which fails when the value is less than the key:

.. code-block:: DaeDaLus 

  for (val = 0; k,v in d) 
    k <= v is true

Traversing with ``map``
-----------------------

DaeDaLus supports another iteration construct, ``map``. This performs an operation on each 
element of a sequence, resulting in a sequence of results. For example, the following code 
doubles each element in an array: 

.. code-block:: DaeDaLus

  map (x in [1:int, 2, 3]) 
    2 * x

The ``map`` construct can be used to parse a sequence of blocks, based on a
sequence of values. For example the following code parses blocks of the form ``0AAA...``, 
with the number of ``'A'`` characters dicated by the input sequence. 

.. code-block:: DaeDaLus 

  map (x in [1, 2, 3]) {
    Match1 '0'; 
    Many x (Match1 'A');
  }

Just as with ``for``, the map construct has an alternative form that includes both 
sequence indexes and values: 

.. code-block:: DaeDaLus 

  map (i,x in [5, 2, 1]) {
    Match1 '0'; 
    len       = ^ { index = i, elem = x };
    something = Many x (Match1 'A');
  }





Commit
------

.. warning::
  ``commit`` is an unstable experimental feature and its behavior may change
  or it may be removed entirely.

Normally, at the point a parser fails, DaeDaLus will backtrack to a choice point 
and try an alternative parser. The ``commit`` guard acts as a cut-point and prevents
backtracking. For example, the following code cannot parse the string ``"AC"`` 
because parsing ``'A'`` and the subsequent ``commit`` will prevent backtracking 
reaching the alternative branch. 

.. code-block:: DaeDaLus 

  Choose1 { 
    { Match1 'A'; commit; Match1 'B' }; 
    { Match1 'A'; Match1 'C' }  -- Can't happen 
  }

The ``try`` construct converts commit failure into parser failure.  A
commit failure will propagate until it hits an enclosing ``try``
construct, or until it escapes the top-level definition.

Type Annotations
----------------

DaeDaLus declarations and expressions may be annotated with explicit types,
which is useful when type inference fails to infer the type of something,
or to improve the readability of the specification.

Annotating an Expression
^^^^^^^^^^^^^^^^^^^^^^^^

Use ``e : t`` to specify that expression ``e`` should have type ``t``.
For example:

.. code-block:: DaeDaLus

  def i_am_a_byte = 1 : uint 8

Note that without the type annotation on the expression ``1`` the
resulting declaration would be polymorphic because literals are overloaded
and may be used at many different types.

Annotating the Result of a Declaration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To specify the result type of a declaration use ``: t`` after the name
(or the parameters, if any) of the declaration like this:

.. code-block:: DaeDaLus

  def also_byte : uint 8 = 1

  def returns_byte x : uint 8 = x


Annotating a Parameter
^^^^^^^^^^^^^^^^^^^^^^

Parameters of declarations may also be annotated with a type:

.. code-block:: DaeDaLus

  def Example (P : uint 8) = P

The previous example specifies that parameter ``P`` is a parser that
will construct a ``uint 8`` semantic value.


Naming Unknown Types
^^^^^^^^^^^^^^^^^^^^

Occasionally it is useful to name a type without specifying it explicitly.
For example:

.. code-block:: DaeDaLus

  def f (x : maybe a) = 1 : a

In the previous example we assume that ``a`` is not the name of any type
that is in scope.  The type annotation ``maybe a`` specifies that the input
should be of type ``maybe a`` for *some* type ``a`` that we can refer to
using the name ``a``, as we do in the body.

.. warning::

  This feature is a bit error prone and may change in the future.
  Common problems are:

  1. Modifying the specification to define type ``a`` changes the
      meaning of a seemingly unrelated declarations
  2. Mistypeing the name of a type could make you think that you've
      speicfied the type, but in fact you just named it.



Coercions
---------

Coercions provide a way to change a semantic value into the corresponding value
of a different type. The general form is ``e as T``, which converts the value of
expression ``e`` into type ``T``. For example, the following code will parse
a byte and pad the resulting value out to a 32-bit unsigned integer. 

.. code-block:: DaeDaLus

  block 
    let i = UInt8 
    ^ i as uint 32

The base form ``e as T`` statically checks that the resulting type has
enough bits to losslessly represent the original value. There are two other
forms, ``as!`` and ``as?`` that can be used when this does not hold true
statically:

* ``e as! T`` is guaranteed to succeed, but may lose information.
  In the case that the original value fits into the target type, the behaviour
  coincides with the lossless version of ``as``. Otherwise, behaviour is
  implementation dependent, but will attempt to do something reasonable.

* ``e as? T`` performs a run-time check that the coercion will not lose
  information. If this holds, behaviour is identical to the lossless version of
  ``as``. Otherwise, the coercion fails and backtracks. 

Note that ``e as T`` and ``e as T`` are values, and ``e as? T`` is a parser.
This is because ``e as? T`` can fail and backtrack, which is only meaningful
in parser expressions.


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

Decisions on a boolean may be made either using `If-then-else`_, by
using `Guards`_, or by using `Case`_.




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
* exclusive-or ``.^.``
* and bitwise-and ``.&.``.

Unsigned numbers can also be appended to other numbers via the
``#`` and ``<#`` operator.  To see the difference between the two,
consider two bitvectors ``(x : uint A)`` and ``(y : uint B)``.
The result of ``x # y`` is a bitvector of type ``A + B`` with
``x`` in the more significatn bits, and ``y`` in the less significant bits.
The result of ``x <# y`` is a bitvector of type ``A`` that contains
``x # y`` but truncated to the ``A`` less significatn bits.


``maybe`` type
--------------

DaeDaLus supports the special polymorphic type ``maybe A``, which has possible 
values ``nothing`` and ``just x``, for some value, ``x`` of type ``A``.

The parser ``Optional P`` will try to parse the input using and produce
a ``maybe`` value.  If ``P`` succeeds with result ``x`` then
``Optional P`` will succeed with ``just x``, and if ``P`` fails, then
``Optional P`` will *succeed* with ``nothing``.

.. code-block:: DaeDaLus 

  def MaybeLetter = Optional (Match1 ('A'..'Z'))

To examine values of the ``maybe`` type you may use `Guards`_ or `Case`_.


Arrays
------

.. todo::
  Describe the interface to ``[ el_type ]``

Association Maps
----------------

.. todo::
  Describe the interface to ``[ key -> value ]``



Stream manipulation
===================

DaeDaLus parsers operate on an *input stream*, which by default is the input
data to the parser. However, the input stream can be manipulated directly. For example, 
we can write a parser function which runs two different parsers on the same stream. 

.. code-block:: DaeDaLus 

  def ParseTwice P1 P2 =
    block
      let cur = GetStream
      p1result = P1
      SetStream cur
      p2result = P2

By manipulating the stream, we can also run a parser on a fixed-size sub-stream.
The following parser parses a size-n chunk which begins with a sequence of
letters, and then is filled with spaces: 

.. code-block:: DaeDaLus 

  def LetterFill n =
    block
      let cur  = GetStream
      let this = Take n cur
      let next = Drop n cur
      SetStream this
      $$ = block
             $$ = Many (Match1 ('A'..'Z'))
             Many (Match1 ' ') 
             END
      SetStream next

It is also possible to directly access the current position in the stream using
``Offset``. This can be used to calculate how many characters were read by a
particular parser: 

.. code-block:: DaeDaLus 

  def OffsetTest = { 
      a = Offset; 
      Match "AA";
      b = Offset; 
      Match "AAA"; 
      c = Offset; 
  }
  -- Result: { a:0, b:2, c:5 } 

The ``arrayStream`` operator converts an array into a stream:

.. code-block:: DaeDaLus 

  def CatStream a b = { 
      SetStream (arrayStream (concat [a, b]));
      Match "AA";
      Match "BBB";
      ^ {}
  }

This example will succeed if the concatenation of the arrays ``a`` and
``b`` starts with the string ``"AABBB"``.


.. _character_classes:

Character Classes
=================


External Declarations
=====================


Bitdata
=======

The ``bitdata`` construct provides a convenient way to break bytes into
groups of bits, which are then combined into a tagged union. 

.. code-block:: DaeDaLus

  bitdata ChooseOption where 
    opt1 = 0x0 : uint 4 
    opt2 = 0x1

  bitdata OptionData where 
    OptionData = { opt : ChooseOption, val : uint 4 }

Bitdata defintions are not parsers, but rather are used by applying coercions to
already parsed bytes. The following code parses a byte, and then checks that the
first four bits select the correct option. 

.. code-block:: DaeDaLus 

  block
    let odat = UInt8 as? OptionData
    case odat of
      OptionData x ->
        case x.opt of
          opt1 -> ^ x.val
          _    -> Fail "Wrong option"

Note that the coercion may fail if the parsed byte does not contain either
``0x0`` or ``0x1`` in its first four bits. In this case, the parser will
backtrack.


Implicit Lifting
================


Implicit Parameters
===================

An *implicit parameter* is a parameter that is automatically
passed along by the system, which helps avoid clutter in specifications.
In DaeDaLus, implicit parameters have names staring with ``?``, for example
``?bigendian``.

Implicit parameters are useful in situations where the value of a parameter
is set once for a given scope, and then the same parameter is just passed
along with no changes to the "leaves" of the specifiction.   This is quite
common in situations where some configration optoins are read once, and then
are just passed along for the rest of a parser.

Here is an example of a function that uses an implicit parameter to concatenate
two bit vectors one way or another:

.. code-block:: DaeDaLus

  def joinWords a b =
    if ?bigendian     -- ?bigendian is an implicit parameter
      then a # b
      else b # a

Parsers automatically inherit the implicit parameters needed by functions
or parsers they use.  For example, here are two parsers that can be used
to parse either big-endian or little-endian words, depending on the value
of the implicit parameter ``?bigendian``:

.. note::
  These parsers use `Implicit Lifting`_ to make them more redbale


.. code-block:: DaeDaLus

  def Word16 = joinWords UInt8 UInt8
  def Word32 = joinWords Word16 Word16

If a ``block`` provides a value for an implicit parameter, then all calls
for the rest of the block will use that value for the parmeter.  For example,
``BEWord16`` *does not* have an implicit parameter:

.. code-block:: DaeDaLus

  def BEWord16 =
    block
      let ?bigendian = true
      Word16    -- `?bigendian` has the value `true`

It is possible to use different values for the same implcit parameter,
as illustarte by the following example:

.. code-block:: DaeDaLus

  def Example =
    block
      -- Just for testing, we set the input stream to a known value
      SetStream (arrayStream (concat [ [0,1,0,0,0,1]
                                     , [1,0,1,0,0,0] ]))

      big =
        -- Here we define the value of an implicit parameter
        -- in all uses for the rest of the block
        block
          let ?bigendian = true
          x = Word16
          y = Word32
      little =
        -- This block uses a different value for the implicit parameter
        block
          let ?bigendian = false
          x = Word16
          y = Word32

Executing ``Example`` results in the following output:

.. code-block:: bash

  { big: { x: 1[16]
         , y: 1[32]
         }
  , little: { x: 1[16]
            , y: 1[32]
            }
  }











