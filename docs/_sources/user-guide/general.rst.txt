General Information
===================

DaeDaLus Input Format
---------------------

DaeDaLus specification files (i.e. ".ddl" files) can only contain ASCII
characters. That means that all string literals and identifiers for
parsers, semantic values, etc. can only contain ASCII characters. At the
time of this writing, DaeDaLus does not support Unicode characters in
its specification files.


Modules and Imports
-------------------

Each DaeDaLus source file defines a module. The module name is
determined by the file name: a file called ``Foo.ddl`` defines a module
called ``Foo``.  Module files may use either the ``.ddl`` or ``.md``
extension.

To use declarations from another module, import it with an ``import``
declaration at the top of the file (before any ``def`` declarations):

.. code-block:: DaeDaLus

  import Foo
  import myHelpers

  def Main = ...

An import brings all declarations from the imported module into scope.
There is no support for selective imports or qualified names---all
imported names are available directly.

When resolving an import, DaeDaLus searches for the corresponding file
in its search path.  For example, ``import Foo`` will look for
``Foo.ddl`` (or ``Foo.md``) in the configured search directories.
