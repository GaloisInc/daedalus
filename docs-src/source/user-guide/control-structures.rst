Control Structures
==================

If-then-else
------------

Booleans may be used to choose between one of two parsers:

.. code-block:: DaeDaLus

  block
    let i = $[ '0'..'9' ]
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
    let i = $[ '0'..'9' ]
    First
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
union types (see :ref:`Tagged Unions`)


.. code-block:: DaeDaLus 

  block
    let res = Choose
                good = $[ 'G' ]
                bad  = $[ 'B' ]

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
                good = $[ 'G' ]
                bad  = $[ 'B' ]
    case res of 
      good -> ^ "Success!"
      bad  -> ^ "Failure!"

A case expression can extract the value from a tagged union. In this case, the 
match should have the form ``pattern var -> result``.

.. code-block:: DaeDaLus 

  block 
    let res = Choose 
                number = $[ '0'..'9' ]
                letter = $[ 'a'..'z' ]
                other  = UInt8
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

.. _for_loops:

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
with the number of ``'A'`` characters dictated by the input sequence. 

.. code-block:: DaeDaLus 

  map (x in [1, 2, 3])
    block
      $[ '0' ]
      Many x $[ 'A' ]

Just as with ``for``, the map construct has an alternative form that includes both 
sequence indexes and values: 

.. code-block:: DaeDaLus 

  map (i,x in [5, 2, 1])
    block
      $[ '0' ]
      len       = ^ { index = i, elem = x }
      something = Many x $['A']


``many`` loops
--------------

``many`` loops are similar to ``for`` loops and the ``Many`` combinator.

  * The loop stops when the parser in the loop's body fails, which is just
    like the ``Many`` combinator.

  * The body is parameterized by a piece of state, which is initialized at
    the start of the loop, and is updated by the result of the body on each
    iteration.

For example, this is how one might parse a base 10 number:

.. code-block:: DaeDaLus

  many (s = 0) (10 * s + Digit)

In the example above, the state variable is ``s`` and is initialized
to ``0``. The loop body is evaluated, and if it succeeds, its value
replaces ``s``, which in the example above means that the loop body's
result is assigned to ``s``, i.e., ``s = 10 * s + Digit``. The result
of the ``many`` is the result of the body of its last successful loop
iteration.

There is also a variant called ``many?`` which is similar to ``Many?`` in
that the loop repeats just enough times to make the parser succeed.

.. warning::
  It is quite easy to write inefficient or ambiguous parser when using
  ``many?`` so it should be avoided, if possible.



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

  First
    { $['A']; commit; $[ 'B' ] }
    { $['A'];         $[ 'C' ] }  -- Can't happen

The ``try`` construct converts commit failure into parser failure.  A
commit failure will propagate until it hits an enclosing ``try``
construct, or until it escapes the top-level definition.

