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
             $$ = Many $[ 'A'..'Z' ]
             Many $[ ' ' ]
             END
      SetStream next

It is also possible to directly access the current position in the stream using
``Offset``. This can be used to calculate how many characters were read by a
particular parser: 

.. code-block:: DaeDaLus 

  def OffsetTest =
    block
      a = Offset
      Match "AA"
      b = Offset
      Match "AAA"
      c = Offset

  -- Result: { a:0, b:2, c:5 } 

The ``arrayStream`` operator converts an array into a stream:

.. code-block:: DaeDaLus 

  def CatStream a b =
    block
      SetStream (arrayStream (concat [a, b]))
      Match "AA"
      Match "BBB"
      ^ {}
  }

This example will succeed if the concatenation of the arrays ``a`` and
``b`` starts with the string ``"AABBB"``.



