Extended Exercise: The PNG Format
=================================

As mentioned very early in the tutorial, DaeDaLus is very good for both text
and binary format specification. We're going to make a big jump in difficulty
to explore the binary format parsing features DaeDaLus has, while also giving
significant practice with the other features already introduced.

The format we'll be specifying is PNG, the Portable Network Graphics image
format, which provides lossless raster-graphics compression; an improvement on
the widely used Graphics Interchange Format (GIF).

Now, the PNG specification is quite long, so we won't be implementing it in its
entirety - in particular, we're mainly focused on the structure of PNG
*chunks*, and do not validate the order/number of chunks other than the header
and end. Furthermore, we do not perform any of the typical CRC validation, a
criticial error detection/correction system that contributes to the reliability
of PNG.

Before diving into the exercises, we'll first have a brief aside on the
high-level guiding principles one should have in mind when deciding what to
include in the parsing stage of a format-processing application - this will
directly address some of the things we're "leaving out" of the DaeDaLus PNG
specification.

In addition, we'll need some of the definitions from the DaeDaLus standard
library, which we did not use at all in the PPM specification - briefly,
this library (which is all implemented in DaeDaLus itself, with some features
not covered in this tutorial) provides a number of convenient types for
handling binary formats in particular.
