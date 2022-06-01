Aside: The DaeDaLus Standard Library
====================================

DaeDaLus, like most modern programming languages, ships with a standard library
of useful parsers and functions. It is quite small, and written entirely in
DaeDaLus itself. This brief section highlights some of the parsers provided,
without digging deeply into their implementation. You'll find this section
useful while working on the extended PNG exercise, particularly for parsing
multi-byte words with the correct endianness.

Endianness
----------

When working with binary data, we must concern ourselves with *endianness* -
the order of bytes within a multi-byte word of data. Many format specifications
define an endianness for their bytes, and your machine's architecture defines
an endianness for, say, multi-byte integers.

The DaeDaLus standard library provides a large handful of parsers that make
working with such data convenient and easy. Names beginnning with ``BE``
parse big-endian words, names beginning with ``LE`` parse little-endian words.
The parsers with names like ``UInt16`` and ``SInt32`` parse *either*
big-endian or little-endian data, depending on the value of an *implicit
parameter*. We haven't discussed implicit parameters in this tutorial - for
more detail about them, see :ref:`implicit parameters`.

First, the unsigned/signed integer parsers that depend on the implicit
parameter ``?bigEndian``:

* ``UInt16``
* ``UInt32``
* ``UInt64``
* ``SInt16``
* ``SInt32``
* ``SInt64``

The always-big-endian variants:

* ``BEUInt16``
* ``BEUInt32``
* ``BEUInt64``
* ``BESInt16``
* ``BESInt32``
* ``BESInt64``

And the always-little-endian variants:

* ``LEUInt16``
* ``LEUInt32``
* ``LEUInt64``
* ``LESInt16``
* ``LESInt32``
* ``LESInt64``

Floating-Point Numbers
----------------------

There are also parsers for floating-point data in the standard library, for
16-, 32-, and 64-bit IEEE-754 standard floating-point numbers. Like with the
integer parsers described above, there are variations that use implicit
parameters, are always big-endian, and are always little-endian.

First, those that depend on the implicit parameter:

* ``HalfFloat``
* ``Float``
* ``Double``

The always-big-endian variants:

* ``BEHalfFloat``
* ``BEFloat``
* ``BEDouble``

And the always-little-endian variants:

* ``LEHalfFloat``
* ``LEFloat``
* ``LEDouble``

Because the PNG specification makes use of a particular endianness for its
words, you'll find many of these parsers useful while completing the exercises.

Guarding and Consuming Everything
---------------------------------

Two very simple parsers in the standard library mimic behavior we've already
covered in earlier sections, but explicitly as named parsers.

The parser ``Guard b`` (where ``b`` is a ``bool``) succeeds if ``b`` is
``true`` and fails otherwise; it's equivalent to ``b is true``.

The parser ``Only P`` succeeds if ``P`` consumes all input. We can write this
instead of the slightly more verbose sequence ``{ $$ = P; END }``.

Manipulating Input Streams
--------------------------

Sometimes (but fortunately not too often), we need to control exactly where we
are in the input stream manually. We can get the current stream using something
like ``let s = GetStream`` inside a parser block.

We can also set the stream with ``SetStream s``. We can use this to parse the
same input more than once, parse fixed-size sub-streams, and generally 'jump
around' in the input we're processing.

The standard library provides some convenient parsers to use these features
in some common ways:

* ``SetStreamAt n s`` sets the current stream to the ``n`` th byte of ``s``,
  which is some stream
* ``Skip n`` advances the current stream ``n`` bytes
* ``Chunk n P`` parses ``n`` bytes using ``P``. Importantly, ``P`` is not
  required to parse all of the bytes, but the stream will still be advanced all
  ``n`` bytes. To make sure ``P`` consumes all ``n`` bytes, use
  ``Chunk n (Only P)``
* ``Bytes n`` gets a chunk of ``n`` raw, unprocessed bytes
* ``LookAhead P`` parses using ``P``, but resets the stream to the current
  position when ``P`` succeeds

And that's all of the standard library, except for some helper functions that
implement these parsers! In the following set of exercises, you'll find these
extremely helpful in writing parsers for the various components of the PNG
format.
