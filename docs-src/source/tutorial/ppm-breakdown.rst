Breaking Down the PPM Specification
===================================

In :ref:`getting started`, we presented this DaeDaLus specification for the PPM
ASCII image format:

.. literalinclude:: examples/plain-ppm.ddl
    :language: DaeDaLus

The following sections will break down this specification in order to introduce
the core features of DaeDaLus, including declarations, parsing primitives,
parser combinators, and some of the facilities for constructing semantic values
that match the structure of the data we're writing layout specifications for.
After reading this section, you should be prepared to write all kinds of data
layouts effortlessly.
