.. _character_classes:

Character Classes
=================

A "character" class describes a set of bytes.

Character classes may be named using definitions like this:

.. code-block:: DaeDaLus

  def $x = CHAR_CLASS

A character class may be used as a parser which matches a single byte,
using the construct ``$[ CHAR_CLASS ]``.   Named character classes,
such as ``$x`` or ``$any`` may be used directly as a byte parser
(i.e.,
writing ``$[ $x ]`` is the same as just writing ``$x`` in a parser context).


+-----------------+-------------------------------------------------------+
| *byte literal*  | Match the literal exactly.                            |
|                 | For example ``0``, ``'a'``, ``0xFF``, ``0b11111111``  |
+-----------------+-------------------------------------------------------+
| ``"abc"``       | Match one of ``'a'``, ``'b'``, or ``'c'``             |
+-----------------+-------------------------------------------------------+
| ``x``           | Match the byte corresponding to the value of ``x``    |
+-----------------+-------------------------------------------------------+
| ``$x``          | Match any byte matched by character class ``$x``      |
+-----------------+-------------------------------------------------------+
| ``$any``        | Match any byte                                        |
+-----------------+-------------------------------------------------------+
| ``'0' .. '9'``  | Match bytes in the given range, inclusive             |
+-----------------+-------------------------------------------------------+
| ``.. '9'``      | Match bytes less than or equal to ``'9'``             |
+-----------------+-------------------------------------------------------+
| ``'0' ..``      | Match bytes greater than or equal to ``'0'``          |
+-----------------+-------------------------------------------------------+
| ``$x | $y``     | Match byte that either match ``$x`` or ``$y``         |
+-----------------+-------------------------------------------------------+
| ``$x - $y``     | Match bytes that are in ``$x`` but not in ``$y``      |
+-----------------+-------------------------------------------------------+
| ``! $x``        | Match bytes that do not match chararcter class ``$x`` |
+-----------------+-------------------------------------------------------+

