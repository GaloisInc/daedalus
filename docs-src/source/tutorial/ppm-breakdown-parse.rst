Breaking down PPM: Primitive Parsing
====================================

Before trying to move on to the other more complex parsers defined in this
specification, it is going to be helpful to outline some of the primitive
parsers made available in the DaeDaLus standard library, pointing to their uses
in the PPM example where appropriate:


``UInt8``
---------

The parser ``UInt8`` parses a single byte of input, failing if there are no
bytes left to consume. When it succeeds, it constructs a semantic value of
type ``uint 8``. The PPM example doesn't use this parser, but later we will
see examples of parsers that do.

.. note::
    Use ``UInt8`` when you need to parse a single specific byte from input.

``$[...]``
--------------

``$[...]`` parses a single byte of input that matches any of the
bytes specified by the content in the square brackets. We can specify
inclusive ranges of bytes, such as ``'0' .. '9'``, or we can specify the
set elements explicitly, as in this example from the PPM specification:

.. literalinclude:: ../examples/plain-ppm.ddl
    :language: DaeDaLus
    :start-after: -- BEGIN PPM_WS
    :end-before: -- END PPM_WS

.. note::
    Use ``$[...]`` when you need to parse one of a finite set of bytes.

``Match ...``
-------------

``Match`` parses a particular sequence of bytes in the input, returning
a semantic value that is an array of bytes (whose type is written
``[uint 8]``.) As an example, we can write ``Match "keyword"`` to match
exactly those bytes corresponding to the string ``"keyword"``. We can
also utilize this parser when working with binary formats, where we may
find it useful to precisely specify the bytes we're expecting, e.g.
``Match [0x00, 0x01]`` which will match the two bytes ``0`` and ``1``.

In the declaration for the parser ``PPM``, we use ``Match "P"`` to consume the
first part of the aforementioned PPM "magic number"; we could have just as well
used ``$['P']`` here, but the meaning in this case is the same.

.. note::
    Use ``Match`` when you need to parse a specific sequence of bytes.

``END``
-------

The parser ``END`` succeeds only if there is no additional input to be
consumed. It results in the 'trivial' semantic value ``{}``. Typically,
a DaeDaLus parser succeeds consuming only a *prefix* of the input, but
adding ``END`` to the end of our parsing sequence means we must consume
the entire input in order to succeed.

The PPM specification does not use the ``END`` parser, so in fact, the
generated parser will consume any input prefixed by a well-formed ASCII
PPM; depending on the use-case, this may or may not be desirable.

.. note::
    Use ``END`` when you want to guarantee full inputs are consumed.

``^ ...``
---------

It is sometimes convenient to 'lift' a semantic value into a parser that
consumes no input and always succeeds, returning that same semantic value;
this is an important part of how data-dependent parsing works in DaeDaLus.
We can accomplish this by placing a ``^`` before any semantic value. For
example, ``^ 'A'`` is a parser that consumes no input and always succeeds,
producing the byte ``'A'`` as a result. If we want to consume no input *and*
return nothing interesting, we may write ``^ {}`` (the same 'trivial' semantic
value returned by the ``END`` parser); DaeDaLus also provides the synonym
``Accept`` for this trivially-succeeding parser for more readability.

The idea here is best shown by example. Consider the declaration of the
``Digit`` parser:

.. literalinclude:: ../examples/plain-ppm.ddl
    :language: DaeDaLus
    :start-after: -- BEGIN PPM_DIGIT
    :end-before: -- END PPM_DIGIT
    :emphasize-lines: 4

Parsers can only be combined with other parsers, so to transform the ASCII
byte we read with ``$[...]`` into the actual digit it represents, we must
write a parser that returns that transformed value. This is exactly the use
for ``^`` since we don't wish to read any additional input.

Parsers defined with ``^`` are called 'pure', because they do not consume any
input (that is, they don't alter the internal parsing state in any way.) We'll
see many more examples of this in the other formats we study.

.. note::
    Use ``^ ...`` to turn semantic values into parsers that don't consume
    input.

``Fail ...``
------------

We can trigger a failure with the ``Fail`` parser, which *always* fails.
Optionally, we can provide a message to this parser which will be printed as
part of the triggered failure; this is how you may indicate to users of your
specifications what exactly went wrong while trying to parse.

The PPM example does not make use of the ``Fail`` parser; it is mostly useful
when performing *validation* of parsed data, which is often better left to
later stages of the processing done on layouts. We'll have some more to say
about this later.

.. note::
    Use ``Fail ...`` to immediately stop parsing with an error message.
