Type Annotations and Coercions
==============================

DaeDaLus declarations and expressions may be annotated with explicit types,
which is useful when type inference fails to infer the type of something,
or to improve the readability of the specification.

Annotating an Expression
------------------------

Use ``e : t`` to specify that expression ``e`` should have type ``t``.
For example:

.. code-block:: DaeDaLus

  def i_am_a_byte = 1 : uint 8

Note that without the type annotation on the expression ``1`` the
resulting declaration would be polymorphic because literals are overloaded
and may be used at many different types.

Annotating the Result of a Declaration
--------------------------------------

To specify the result type of a declaration use ``: t`` after the name
(or the parameters, if any) of the declaration like this:

.. code-block:: DaeDaLus

  def also_byte : uint 8 = 1

  def returns_byte x : uint 8 = x


Annotating a Parameter
----------------------

Parameters of declarations may also be annotated with a type:

.. code-block:: DaeDaLus

  def Example (P : uint 8) = P

The previous example specifies that parameter ``P`` is a parser that
will construct a ``uint 8`` semantic value.


Naming Unknown Types
--------------------

Occasionally it is useful to name a type without specifying it explicitly.
For example:

.. code-block:: DaeDaLus

  def f (x : maybe ?a) = 1 : ?a

The type annotation ``maybe ?a`` specifies that the input should be of type
``maybe ?a`` for *some* type ``?a`` that we can refer to
using the name ``?a``, as we do in the body.

.. warning::

  The notation ?x is used for two unrelated features:  in types it refers
  to some unknown type, as in the previous example;  in parsers and value
  declarations it refers to :ref:`Implicit Parameters`.


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

Note that ``e as T`` and ``e as! T`` are values, and ``e as? T`` is a parser.
This is because ``e as? T`` can fail and backtrack, which is only meaningful
in parser expressions.
