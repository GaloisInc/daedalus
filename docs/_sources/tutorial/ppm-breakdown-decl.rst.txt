Breaking down PPM: Declarations
===============================

Starting from the very beginning of the PPM specification, we have:

.. code-block:: DaeDaLus

    def Main = {
      $$ = PPM;
    }

This is a *declaration* for the name ``Main``. Declarations in general are
indicated by the keyword ``def``, followed by the name being declared,
any formal parameters, an equals sign, and finally the definition itself.

As in most programming langauges, declarations allow us to name certain
entities that we may wish to refer to later, often with the intent of reducing
duplication and helping readers understand the ideas we're trying to express
(in the immortal words of Guido Van Rossum, creator of Python: Code is read
more often than it is written.) In this particular case, the name ``Main``
indicates to the DaeDaLus interpreter and backend that this is the entry point
of the parser being defined. All layout specifications must declare a ``Main``
symbol.

.. warning::

    In DaeDaLus, declarations behave differently from what you might be used to
    if you are unfamiliar with pure functional programming. In such languages,
    variables are *immutable*: Once defined, they cannot be re-defined. While
    at first this may seem limiting, it makes it far easier to reason about the
    behavior of code since each name always refers to the same thing - anywhere
    we see the name used, we can substitute its one precise definition without
    changing the meaning of the program or having to be concerned about changes
    to any global state.

Deducing Types From Names
-------------------------

Though it's probably not obvious, there's actually another piece of information
we can deduce from this declaration: What type of entity ``Main`` refers to.

In DaeDaLus, there are three sorts of entities that may be declared: *parsers*,
*semantic values*, and *character classes*.

Parsers **always** have a name starting with an uppercase letter; so, in the
example declaration we're currently looking at, ``Main`` is a parser.

In contrast, semantic values **always** have a name starting with a lowercase
letter. Looking ahead in the PPM example, by this convention, the declaration
of ``addDigit`` specifies a semantic value; in particular, a function from two
semantic values to another semantic value.

Finally, character classes **always** have a name starting with ``$``. The PPM
example does not specify any character classes, but we'll have more to say
about them later.

.. note::
    In summary:

    * Parser names begin with uppercase letters
    * Semantic value names begin with lowercase letters
    * Character class names begin with ``$``

    Keeping these rules in mind will save you a lot of trouble debugging in the
    future!

Parameterized Declarations
--------------------------

The next declaration in the PPM specfication shows that a declaration may be
*parameterized*:

.. code-block:: DaeDaLus

    def Token P = {
      $$ = P;
      Many (1..) WS;
    }

Parameter names follow the same rules outlined above: Uppercase names indicate
parser parameters, lowercase names indicate semantic value parameters, and
names starting with ``$`` indicate character class parameters. In this example,
since ``P`` is capitalized, we know it is a parser parameter.

The ``Token`` parser also demonstrates similarities between DaeDaLus and
*parser combinator* libraries such as ``parsec`` for the Haskell programming
language. Rather than having to write complex parsing algorithms from scratch,
a library of primitive parsers and higher-order combining operations are
provided as building blocks. If you're already familiar with these sorts of
parsing libraries, you're well on your way to being a productive DaeDaLus user!
