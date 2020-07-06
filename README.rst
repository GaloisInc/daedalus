DaeDaLus
========

Daedalus is a flexible data description language for generating parsers
with data dependencies.


Installing Haskell
------------------

Daedalus is implemented in Haskell, so to build it you'd need to setup
a Haskell environment.  An easy way to get this setup is to use ghcup_.
You need a Haskell compiler (recommended `GHC 8.8.3`), and a Haskell package
installer (recommended `Cabal 3.2`).

.. _ghcup: https://www.haskell.org/ghcup/


Building the Daedalus Interpreter
---------------------------------

The Daedalus interpreter, called ``daedalus`` is a good way to experiment
with, and learn the Daedlus language.  To build and install the interpreter
you may use the command:

.. code-block:: bash

    cabal install exe:daedalus --installdir=DIR \
                               --overwrite-policy=always

This instructs ``cabal`` to build Daedlus and place a link to the binary
in directory ``DIR``.  The flag ``--overwrite-policy`` is optional and
instructs ``cabal`` to overwite existing links with the same name.


Running the Interpreter
-----------------------

To run ``daedalus`` you need a Daedalus specification describing the
parser, and a file that should be parsed.  For example, you could try
the following, assuming ``daedalus`` is in your path:

.. code-block:: bash

  daedalus tests/midi/midi.ddl -i tests/midi/inputs/moz_k299.midi

The first argument to daedalus is a specification describing the ``midi``
format and the ``-i`` flag specfies the input that should be parsed.
If the input is parsed successfully, then ``daedalus`` will display the
resulting semantic value.  Otherwise, you should see a parse error describing
what went wrong.


Setting up Your Editor
----------------------

Directory ``syntax-hilight`` contain Daedalus syntax hilighting files for
common editors.


More about the Daedalus Language
---------------------------------

The documentation for ``Daedlus`` is not yet complete, but you may read
more about the language in ``docs/UserGuide.pdf``.  The ``tests`` directory
contains numerous small examples of ``Daedlus`` specifications, of particular
interest might be subdirectories ``ppm`` (PPM image format),
``sexp`` (S-expressions) and ``midi`` (MIDI messages).


Generating Parsers
------------------

At present we can translate Daedalus specifications into Haskell, so that
the resulting parsers may be intergrated with Haskell applications.
We are working on adding support for generating parsers in other languages
as well.  The parser generation functionality is very much in development
at the moment.

The repositry contains two examples of Haskell applications using Daedalus
parsers.  They are both validators, for the ICC and PDF format respectively.
You may install them using the commands:

.. code-block:: bash

    cabal install exe:icc-parser    --installdir=DIR
    cabal install exe:pdf-hs-driver --installdir=DIR

The source code for the ICC validator is in ``icc-driver`` and the source
code for the PDF validator is in ``pdf-driver``.


