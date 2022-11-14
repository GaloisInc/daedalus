Extended Exercise: Defining Helpful Utilities
=============================================

The layout specification for PNG chunks can be found
`here <https://www.w3.org/TR/2003/REC-PNG-20031110/#11Chunks>`_ - you shouldn't
actually need to read much of this document, as we'll present any relevant
information within the exercises themselves. If you want to write a full PNG
implementation at some point, though, this specification has everything you'll
need.

First of all, fire up your favorite text editor, and create a new file named
``png.ddl``. This is where you'll write the format specification - the first
line of your file, after any comments you wish to add to introduce the module,
should be:

.. literalinclude:: ../examples/png.ddl
    :language: DaeDaLus
    :start-after: -- BEGIN PNG_IMPORT
    :end-before: -- END PNG_IMPORT

This will load all of the standard library features covered in the previous
section, which you'll start using right away.

Domain-Specific Parser Names
----------------------------

For total clarity in code, it is often useful to give new names to things that
are otherwise quite generic (e.g. to represent lengths, we may be using
floating-point numbers - but the type name ``length`` is more informative than
``float``, so we might want to *alias* these names.)

In PNG, there are a number of examples of this being useful - unsigned 32-bit
words are used for a number of very different components.

**Exercise:** Define two parsers, ``Length`` and ``Crc``, that each parse a
big-endian, 32-bit, unsigned integer.

.. dropdown:: Hint
    :color: info

    You may want to review the section
    :ref:`aside: the daedalus standard library` if you're not sure how to deal
    with the endianness.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_LC
        :end-before: -- END PNG_LC

**Exercise:** There are a couple of places in the PNG specification where we
have 1-bit *flags*, indicating whether some option is enabled or not. Write a
parser ``FLAG`` that matches a byte that is ``0`` or ``1``.

.. dropdown:: Solution
    :color: warning

    We use a *character class* to write this very succinctly:

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_FLAG
        :end-before: -- END PNG_FLAG

    For brevity, we'll prefer this syntax in the other provided solutions.

Null-terminated Strings
-----------------------

A *null-terminated string* is an array of characters terminated with the null
character (ASCII codepoint ``0``). These are also known as **C strings**, as
this is the representation used in that language's standard string manipulation
library.

PNG makes use of null-terminated strings in a few places, so we need to be able
to parse them. In particular, we need to be able to parse both strings within a
specific range of sizes *and* strings of unbounded length.

**Exercise:** Define a parser, ``NullChar``, that parses the ASCII null byte.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_NC
        :end-before: -- END PNG_NC

**Exercise:** Define a parser, ``NonNullChar``, that parses one non-null
ASCII byte.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_NNC
        :end-before: -- END PNG_NNC

**Exercise (Challenging):** Define a parser, ``OMany``, that behaves like
``Many``, but takes the integer arguments as ``maybe`` values. Your parser
should satisfy the following *laws*:

* ``OMany nothing nothing       P = Many P``
* ``OMany nothing (just max)    P = Many (..max) P``
* ``OMany (just min) nothing    P = Many (min..) P``
* ``OMany (just min) (just max) P = Many (min..max) P``

.. dropdown:: Hint
    :color: info

    The above equations are the hard part! Your job is to write the 'glue' to
    bring it all together.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_OMANY
        :end-before: -- END PNG_OMANY

    Note that the right-hand sides of each case arm is the right-hand side of
    one of the laws - this form of algebraic specification is very useful when
    writing functional code, as all that was left at the end was writing the
    appropriate pattern-matching code to cover the cases of our laws.

**Exercise (Challenging):** Define a parser, ``NTString``, that parses a
null-terminated string between ``min`` and ``max`` characters in length, if
bounds are provided (i.e. the bounds should be ``maybe`` values.) The null
character should not be included in the character count.

.. dropdown:: Hint
    :color: info

    The ``OMany`` parser you wrote in the previous exercise should be extremely
    helpful.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_NT
        :end-before: -- END PNG_NT

Common Structures
-----------------

Time and RGB color are both critical parts of the PNG specification; the latter
is obvious given this is an image format, but the former is perhaps a little
surprising: By the format's definition, PNG chunks can carry last-modified time
data.

**Exercise:** Define a parser ``RGB`` that consumes three bytes and returns
them in a structure with fields ``red``, ``green``, and ``blue``. The parsed
bytes should be assigned to those fields in that order (i.e. ``red`` is the
first byte, ``green`` the second, and ``blue`` the third.)

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_RGB
        :end-before: -- END PNG_RGB

    While the order of fields in a structure doesn't matter in general, when
    writing DaeDaLus specifications, we must be sure to write the fields in the
    order in which things will be parsed. If there is a clearer order to write
    the structure itself in, you can feel free to use the de-sugared form of
    sequence parsing discussed in an earlier section.

**Exercise:** The time format used by PNG is given by this table:

.. list-table:: PNG Time Stamps
    :header-rows: 1

    * - Year
      - Month
      - Day
      - Hour
      - Minute
      - Second
    * - 2 bytes
      - 1 byte (1 - 12)
      - 1 byte (1 - 31)
      - 1 byte (0 - 23)
      - 1 byte (0 - 59)
      - 1 byte (0 - 60)

Write a parser ``UTCTime`` that produces a structure with these fields with the
given value constraints. The order of columns in the table is the order the
fields should be parsed, and the year should be parsed in big-endian order.

.. dropdown:: Hint
    :color: info

    Remember that you can use the syntax ``$[n .. m]`` to parse one byte with
    a value between ``n`` and ``m``, inclusive.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :start-after: -- BEGIN PNG_UTC
        :end-before: -- END PNG_UTC

    In case you're wondering why the ``second`` field allows for a value of 60:
    It's to allow for leap-seconds, according to the PNG specification!
