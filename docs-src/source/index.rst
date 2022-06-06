*******************
DaeDaLus User Guide
*******************

DaeDaLus is a domain specific language for specifying parsers.  It supports
data dependent parsing, which allows a parser's behavior to be affected by
the semantic values parsed from other parts of the input.
This allows for a clear, yet precise, specification of many binary formats.

.. toctree::
  :caption: Using the Tools

  using-daedalus

.. toctree::
  :maxdepth: 5
  :caption: Daedalus Language

  declarations
  parsers
  control-structures
  types
  semantic-values
  streams
  character-classes
  external
  bitdata
  lifting
  implicit-parameters

.. toctree::
  :maxdepth: 5
  :caption: Daedalus Tutorial

  tutorial/getting-started
  tutorial/ppm-breakdown-decl
  tutorial/ppm-breakdown-parse
  tutorial/ppm-breakdown-comb
  tutorial/ppm-breakdown-expr
  tutorial/extended-ex-intro
  tutorial/extended-ex-parsers
  tutorial/extended-ex-stdlib
  tutorial/extended-ex-utils
  tutorial/extended-ex-chunks
  tutorial/extended-ex-solution