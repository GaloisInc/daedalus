Validation in Parsing
=====================

Any program that consumes input involves typically some kind of parsing.
Some input formats are so simple that we can easily write the logic
needed to consume them without advanced tools like DaeDaLus, and some
are so complex that extensive validation and transformations are
required to use the final result; in those cases it may be difficult
to tell where the line should be between the parser and any additional
processing and validation steps.

We take the position that the job of the parser is to consume an input
stream and produce a data structure that conforms to the *actual*
shape of the input, while leaving deeper validation for whatever is
consuming the parsed data. This may seem simple enough, but what if some
invariants must hold true for the input, such as invariants depending on
more complex logic than what is needed to merely parse the input?

As an example, consider that in many programming languages, it is
necessary to declare a name before it can be used (C and C++ are good
examples of languages like this). We must then consider the question of
whether a parser for C be responsible for checking that any encountered
name has previously been declared, or whether that is better left to a
separate analysis once we know the program is syntactically valid.

Consider another example: the official PNG specification requires that
the number and order of chunks be checked. Should this requirement be
expressed in our DaeDaLus specification?

Answering these questions can often be a matter of taste. Another project from
Galois, `Blocktorok <https://github.com/GaloisInc/blocktorok>`_, checks that
user-defined names in that language's schemas are defined before use while
parsing. Other languages defer this check to after parsing, usually during
type-checking or some other static analysis.

This latter strategy is preferable in the sense that concerns of the
application are being separated more cleanly, but the former has the
benefit of providing excellent error messages without needing to carry
source information into later stages of the data processing. As with
most problems in computer science and engineering, it comes down to
tradeoffs.

To put this another way: the additional checking you do while parsing,
if any, will depend on your needs. A good rule of thumb is that if
it's cheap to check something while parsing, and doesn't require
overly-complex logic, checking it at parse time may be appropriate. An
example from the PPM specification is that where we used a guard to
guarantee that the magic number parsed was the exactly correct value.
An example of the opposite approach is that since the CRC check on PNG
entails fairly involved math, the parser assumes that will be performed
once the image is known to be otherwise well-formed.

When writing your own specifications, be sure to be critical of these
considerations. Adding too much analysis to the parser itself can
degrade performance make maintenance and updates more difficult or
costly in the future.
