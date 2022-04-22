Getting Started
===============

**DaeDaLus** is a domain-specific programming language for specifying
*parsers*. It supports *data-dependent* parsing, meaning that the behavior of
a parser can be effected by the *semantic values* parsed from other parts of
the input. This makes DaeDaLus extremely well-suited for the concise and
precise specification of both text and binary formats.

These parser specifications may be directly applied to inputs via the
``daedalus`` interpreter to produce serialized representations of the resulting
semantic values, *or* the specifications may be compiled to either Haskell or
C++ sources for parsers to be used in larger software ecosystems.

.. note:: This tutorial assumes that you have a working knowledge of common
          programming concepts and are comfortable working at the command-line.
          Familiarity with functional programming will be particularly helpful,
          but is not a strict requirement - any concepts needed will be covered
          in the relevant sections.

Installation
------------

DaeDaLus currently has no binary releases available for installation, but can
be easily built and installed from source.

Since DaeDaLus is implemented in Haskell, you will need a suitable Haskell
environment; the easiest way to set this up is with
`ghcup <https://www.haskell.org/ghcup/>`_. Simply follow the instructions on
that page, and you will have the ability to install the necessary components to
build DaeDaLus.

At the time of this writing, we recommend that you use ``ghcup`` to install
GHC 8.10.7 and Cabal 3.6.2.0:

.. code-block:: bash

    > ghcup install ghc 8.10.7 && ghcup set ghc 8.10.7
    > ghcup install cabal 3.6.2.0 && ghcup set cabal 3.6.2.0

If you prefer, you can run ``ghcup tui`` to interactively install and set
default GHC and Cabal versions.

With Haskell installed, clone
`the DaeDaLus repository <https://github.com/GaloisInc/daedalus>`_. Once
cloned, ``cd`` into the repository root and run:

.. code-block:: bash

    > cabal install exe:daedalus --installdir=DIR \
                                 --overwrite-policy=always

This will build and install the executable ``daedalus``, placing links to the
executable in the directory ``DIR``. We recommend setting this installation
directory to something on your ``PATH``, so that the interpreter can be invoked
simply as ``daedalus`` at the command-line -- the remainder of this tutorial
assumes this setup for brevity.

DaeDaLus Syntax Highlighting / Editing Modes
--------------------------------------------

The DaeDaLus repository ships with a major Emacs mode definition and syntax
highlighters for both Vim and VSCode. Please see your editor's documentation
for details on how to install these extra features.

Your First DaeDaLus Specification
---------------------------------

The Portable PixMap Format
^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's dive right in and write a format specification for the ASCII Portable
PixMap format, PPM.

PPM is a simple image format designed to make it easy to exchange between
different platforms. For the purposes of this introduction, we'll be looking
specifically at the ASCII PPM format, which describes color RGB images in a
human-readable format. Informally, this format consists of:

1. A magic number identifying the file type
2. The dimensions of the image (width then height)
3. The maximum color value
4. A 'matrix' of RGB triples for each pixel defined in row-major order

The format also allows for single-line comments, but we will ignore these for
now.

An Example PPM Image
^^^^^^^^^^^^^^^^^^^^

Here is a small example of a PPM image:

.. code-block::

    P3
    4 4
    15
     0  0  0    0  0  0    0  0  0   15  0 15
     0  0  0    0 15  7    0  0  0    0  0  0
     0  0  0    0  0  0    0 15  7    0  0  0
    15  0 15    0  0  0    0  0  0    0  0  0

We can match this up with the format description given above:

1. The magic number is ``P3``, indicating an ASCII RGB image
2. The width and height are both ``4``
3. The maximum color value is ``15``
4. There is a four-by-four grid of triples, one triple per pixel

The DaeDaLus PPM Specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our goal now is to provide a DaeDaLus specification for this format, so that we
may parse well-formed PPM values into semantic values for further processing in
Haskell or C++ (you might imagine we are writing a program to transform images
represented in this PPM format.) That specification is provided here wholesale;
don't worry if it doesn't make sense just yet! In the next section, we'll break
this specification down line-by-line. Without further ado:

.. code-block:: DaeDaLus

    def Main = {
      $$ = PPM;
    }

    def Token P = {
      $$ = P;
      Many (1..) WS;
    }

    def PPM = {
      Match "P";
      @version = Token Natural;
      version == 3 is true;
      width  = Token Natural;
      height = Token Natural;
      maxVal = Token Natural;
      data   = Many height (Many width RGB);
    }

    def RGB = {
      red   = Token Natural;
      green = Token Natural;
      blue  = Token Natural;
    }

    def WS = Match1 (0 | 9 | 12 | 32 | '\n' | '\r')

    def Natural = {
      @ds = Many (1..) Digit;
      ^ for (val = 0; d in ds) (addDigit val d);
    }

    def Digit = {
      @d = Match1 ('0' .. '9');
      ^ d - '0';
    }

    def addDigit val d =
      10 * val + (d as uint 64)
