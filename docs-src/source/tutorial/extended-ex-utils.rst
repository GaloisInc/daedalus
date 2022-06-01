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

.. code-block:: DaeDaLus

    import Daedalus

This will load all of the standard library features covered in the previous
section, which you'll start using right away.


