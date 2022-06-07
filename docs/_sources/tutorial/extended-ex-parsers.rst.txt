Aside: What To (Not) Do While Parsing
=====================================

Any program that consumes input involves some kind of parsing, even if you
wouldn't typically think of them that way. Some input formats are so simple
that we can easily write the logic needed to consume them without any fancy
tools like DaeDaLus, and some are so complex that we might question exactly
what the job of the parser is and should be.

In general, the job of the parser is to consume an input stream and produce (a)
data structure(s) that conform to the *actual* shape of the input (since the
stream is likely a linear collection of characters or tokens.)

Seems simple enough - but what if some invariants must hold true for the input,
invariants depending on more complex logic than "this input has the right
shape"?

For example: In many programming languages, it is necessary to declare a name
before it can be used (C and C++ are good examples of languages like this.)
Should a parser for C be responsible for checking that any encountered name has
previously been declared, or is this better left to a separate analysis once we
know the program is syntactically valid?

Another example: The official PNG specification requires that the number and
order of chunks be checked. Are we wrong to not include this in our DaeDaLus
specification?

Answering these questions can often be a matter of taste: Another project from
Galois, `Blocktorok <https://github.com/GaloisInc/blocktorok>`_, checks that
user-defined names in that language's schemas are defined before use while
parsing. Other languages defer this check to after parsing, usually during
type-checking or some other static analysis.

This latter strategy is preferrable in the sense that concerns of the
application are being separated more cleanly, but the former has the benefit of
providing excellent error messages without needing to carry source information
into later stages of the data processing. As with most problems in computer
science and engineering, it comes down to tradeoffs.

Put another way: the additional checking you do while parsing, if any, will
depend on your needs. A good rule of thumb is: If it's cheap to check something
while parsing, and doesn't require overly-complex logic, go ahead and check. A
great example came from the PPM example where we used a guard to guarantee that
the magic number parsed was the exactly correct value. Another great example,
from the other side: The CRC check on PNG is fairly involved math, so should be
performed once the image is known to be otherwise well-formed.

When writing your own specifications, be sure to be critical of these
considerations - adding too much analysis to the parser itself can slow it down
and make maintenance/updates dramatically more difficult to perform in the
future.
