Declarations
============

A DaeDaLus specification consists of a sequence of *declarations*.
Each declaration can specify either a *parser*, a *semantic value*, or
a *character class*.  Parsers may examine and consume input, and have
the ability to fail.  If successful, they produce a semantic value.
Character classes describe sets of bytes, which may be used to define
parsers, and will be discussed in more detail in section
:ref:`Character Classes`.

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

  def Example n P =
    if n > 0
      then P
      else UInt8

This declares a parser called ``Example`` with 2 parameters, ``n`` and ``P``.
Note that the parameters are simply separated by space, and usually
there is no need to provide type annotations as DaeDaLus can infer the types
based on the naming rules and the uses in the definition.
