Extended Exercise: The PNG Format
=================================

As mentioned very early in the tutorial, DaeDaLus is very good for both
text and binary format specification. We're going to make a big jump in
difficulty to explore the binary format parsing features DaeDaLus has
while also giving significant practice with the other features already
introduced.

The format we'll be specifying is PNG, the Portable Network Graphics
image format, which provides lossless raster-graphics compression.

The PNG specification is quite long, so we won't be implementing it in
its entirety in this exercise. In particular, we'll focus primarily
on the structure of PNG *chunks* and we will not validate the order
or number of chunks other than the header and end. Furthermore, we
will not perform any of the typical CRC validation, a criticial error
detection/correction system that contributes to the reliability of PNG.

We recommend reading :ref:`Validation in Parsing` on some high-level
guiding principles one should have in mind when deciding what to include
in the parsing stage of a format-processing application. This will
directly address some of the things we're omitting from our development
of a PNG parsing specification.

Before we get started with the PNG specification itself, we'll introduce
some useful definitions from the DaeDaLus standard library that we did
not use at all in the PPM specification. The standard library (which
is all implemented in DaeDaLus itself) provides a number of convenient
types for handling binary formats in particular.
