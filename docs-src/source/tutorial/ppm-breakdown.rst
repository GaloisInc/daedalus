Breaking Down the PPM Specification
===================================

In :ref:`getting started`, we presented this DaeDaLus specification for the PPM
ASCII image format:

.. literalinclude:: examples/plain-ppm.ddl
    :language: DaeDaLus

While this particular example does not show off all the features of DaeDaLus,
it serves as a perfect introduction to the most foundational concepts. Starting
from the very beginning, we have:

.. code-block:: DaeDaLus

    def Main = {
      $$ = PPM;
    }

This is a *declaration* for the name ``Main``. As in most programming
langauges, declarations allow us to name certain entities that we may wish to
refer to later, often with the intent of reducing duplication and helping
readers understand the ideas we're trying to express. In this particular case,
the name ``Main`` indicates to the DaeDaLus interpreter and backend that this
is the entry point of the parser being defined.

.. warning::

    In DaeDaLus, declarations behave differently from what you might be used to
    if you are unfamiliar with pure functional programming. In such languages,
    variables are *immutable*: Once defined, they cannot be re-defined. While
    at first this may seem limiting, it makes it far easier to reason about the
    behavior of code since each name always refers to the same thing.

Though it's probably not obvious, there's actually another piece of information
we can deduce from this declaration: What type of entity ``Main`` refers to.

In DaeDaLus, there are three sorts of entities that may be declared: *parsers*,
*semantic values*, and *character classes*.

Parsers **always** have a name starting with an uppercase letter; so, in the
example declaration we're currently looking at, ``Main`` is a parser.

In contrast, semantic values **always** have a name starting with a lowercase
letter. Looking ahead in the PPM example, by this convention, the declaration
of ``addDigit`` specifies a semantic value.

Finally, character classes **always** have a name starting with ``$``. The PPM
example does not specify any character classes, but we'll have more to say
about them later.

.. note::
    In summary:

    * Parser names begin with uppercase letters
    * Semantic value names begin with lowercase letters
    * Character class names begin with ``$``
