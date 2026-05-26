Standard Library
================

DaeDaLus provides a standard library module called ``Daedalus`` that can
be imported with:

.. code-block:: DaeDaLus

  import Daedalus

Multi-Byte Integer Parsers
--------------------------

These parsers read multi-byte integers from the input.  They use the
implicit parameter ``?bigEndian`` to determine byte order:

================================ ==============================================
Parser                           Description
================================ ==============================================
``UInt16``                       Parse a 16-bit unsigned integer
``UInt32``                       Parse a 32-bit unsigned integer
``UInt64``                       Parse a 64-bit unsigned integer
``SInt16``                       Parse a 16-bit signed integer
``SInt32``                       Parse a 32-bit signed integer
``SInt64``                       Parse a 64-bit signed integer
================================ ==============================================

There are also variants with a fixed byte order:

================================ ==============================================
Parser                           Description
================================ ==============================================
``BEUInt16``, ``BEUInt32``, ...  Big-endian (most significant byte first)
``LEUInt16``, ``LEUInt32``, ...  Little-endian (least significant byte first)
================================ ==============================================

Floating-Point Parsers
----------------------

================================ ==============================================
Parser                           Description
================================ ==============================================
``HalfFloat``                    Parse a 16-bit half-precision float
``Float``                        Parse a 32-bit single-precision float
``Double``                       Parse a 64-bit double-precision float
================================ ==============================================

These also have ``BE`` and ``LE`` variants (e.g., ``BEFloat``, ``LEDouble``).

Utility Combinators
-------------------

================================ ==============================================
Combinator                       Description
================================ ==============================================
``Guard b``                      Succeed if ``b`` is true, fail otherwise
``GuardMsg b s``                 Like ``Guard`` but with a custom error message
``When P x``                     Parse ``P``, discard its result, return ``x``
``Default x P``                  Parse ``P``; if it fails, return ``x`` instead
``Only P``                       Succeed only if ``P`` consumes all input
``Count P``                      Count how many times ``P`` succeeds
``ManyStart P Q``                Match one ``P`` followed by zero or more ``Q``
================================ ==============================================

Numeric Utilities
-----------------

================================ ==============================================
Function                         Description
================================ ==============================================
``numBase base ds``              Evaluate a digit array in the given base
``min x y``                      The smaller of two values
``max x y``                      The larger of two values
================================ ==============================================

Stream Manipulation
-------------------

================================ ==============================================
Combinator                       Description
================================ ==============================================
``SetStreamAt n s``              Set the stream to the ``n``-th byte of ``s``
``Skip n``                       Advance the current stream by ``n`` bytes
``Chunk n P``                    Parse the next ``n`` bytes using ``P``
``Bytes n``                      Get the next ``n`` bytes as a raw array
``LookAhead P``                  Parse with ``P`` without consuming input
``WithStream s P``               Parse ``s`` with ``P``, leaving current stream
                                 unchanged
================================ ==============================================

Testing Combinators
-------------------

These are useful for writing test cases within DaeDaLus:

====================================== ========================================
Combinator                             Description
====================================== ========================================
``TestAccepts Parser input``           Succeeds if ``Parser`` consumes all of
                                       ``input``
``TestAcceptsPrefix Parser input``     Succeeds if ``Parser`` accepts a prefix
                                       of ``input``
``TestParses Parser input expected``   Succeeds if ``Parser`` produces
                                       ``expected`` from ``input``
``TestFails Parser input``             Succeeds if ``Parser`` rejects ``input``
====================================== ========================================
