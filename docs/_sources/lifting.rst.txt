Implicit Lifting
================

At its core, DaeDaLus has three semantics categories:

  * semantic values (names start with a lower case letter),
  * character classes (names start with ``$``), and
  * parsers (names start with an upper case letter).

Various language constructs have expectations about the kinds of entities
they would be used with.  For example:

  * the operator ``+`` expects two semantic values;
  * the construct ``Match1`` expects a character class;
  * the construct ``Many`` expects a parser.

DaeDaLus allows more flexible expressions where language constructs and
declarations may be used with arguments that do not exactly match the expected
categories, subject to the following conversions:

+-------------------+-----------------+-----------------------------------------+
| From              | Context         | Effect                                  |
+-------------------+-----------------+-----------------------------------------+
| Value ``x``       | Char Class      | ``[x]``  singleton char class           |
+-------------------+-----------------+-----------------------------------------+
| Value ``x``       | Parser          | ``^ x``  succeed with the value         |
+-------------------+-----------------+-----------------------------------------+
| Char Class ``$x`` | Value           | Not allowed                             |
+-------------------+-----------------+-----------------------------------------+
| Char Class ``$x`` | Parser          | ``Match1 $x`` match the char class      |
+-------------------+-----------------+-----------------------------------------+

In addition to these conversions, DaeDaLus also allows using parsers in
contexts where semantic values are expected, as long as the overall expression
was already a parser.  For example, while ``+`` normally expects two semantic
values, we also allow expressions like ``P + Q`` where ``+`` is used with two
*parsers* as arguments.

The meaning of such expressions is that we first execute the parsers
in left-to-right order, and then apply the function to their *results*.
For example, ``P + Q`` is exactly equivalent to the following parser:

.. code-block:: DaeDaLus

  block
    let x = P
    let y = Q
    ^ (x + y)



