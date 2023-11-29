# Polyglot Detection and Generation

Polyglots are documents that can be parsed as more than one data format.
Identifying polyglots is important, because they can be used as an attack
vector to smuggle malicious code, such as [disguising PHP code as a JPG
file](https://www.keysight.com/blogs/tech/nwvs/2020/07/23/exploiting-php-phar-deserialization-vulnerabilities-part-1).

Two file formats can combine to form polyglots if they admit complementary
cavities.  A cavity is a location in a file that can have arbitrary (or mostly
arbitrary) content.  For example, PDF files are well known for having a cavity
at the start of the file, because PDF parsers discard data until they find
`%PDF`, the start-of-file marker.

We can use Daedalus and Talos to find cavities in file formats, specify how
formats combine to form polyglots, and generate example polyglot files.

## Finding Cavities

[These
notes](https://docs.google.com/document/d/1Zcrn7_AH1EGbziyQcdNAaYIVljaw5EchNcI1Gp8iUHo/edit#heading=h.an7ziriw2ubc)
define an operational semantics for Daedalus parsers, identify characteristics
of data format cavities, and show how to use cavity characteristics to compose
polyglots.

TODO: Fill details of the detection algorithm here and how to run it.

## Specifying and Generating Polyglots

A "polyglot skeleton" is a format-independent Daedalus specification that
describes how cavities combine to form polyglots.  Albertini et. al. describe a
number of polyglot combinations described
[here](https://eprint.iacr.org/2020/1456).  We have created two polyglot
skeletons from them.

1. [Stacks](stack), formed by concatenating files with prefix and suffix cavities.
1. [Conditional cavities](conditional-cavity), formed by combining a file with a conditional cavity with a location-independent file.

We instantiate polyglot skeletons for pairs of file formats, provided they
have the appropriate cavities.  Given such an instantiation, Talos can generate
sample polyglot files.

## Examples

The [stack](stack) and [conditional-cavity](conditional-cavity) directories
contain instantiated polyglot skeletons for PDF, PHAR, Zip, and other file
formats, along with instructions for generating sample polyglots.

We have used this methodology to partially reproduce the results of
[Mitra](https://github.com/corkami/mitra) as well as data formats beyond those
in Mitra.

```
                          Delayed                 Magic at offset zero,                  No appended
      Any offset  Cavities start                 tolerated appended data                     data    Footer

        Z 7 A R   P I D T   P M   A B B C C E E F F G G I I I I J J N O P L P P R R T W   B J P P W   I X
        i Z r A   D S C A   S P   R M Z A P B L L l I Z C C D L P P E G S N E N I T I A   P a C C A   D Z
        p   j R   F O M R     4     P 2 B I M F V a F   C O 3 D 2 G S G D K   G F F F D   G v A A S   3
                                          O L     c         v A                 F   F       a P P M   v 2

Mitra:
Zip     . X X X   X X X X   X X   X X X X X   X X X X X   X     X X   X X   X X   X X X     X             30
PDF     X X X X   . X X X   X X   X X X X X   X X X X X   X     X X   X X   X X   X X X     X             30

Other:
Phar    X X X X   X X X X   X X   X X X X X   X X X X X   X     X X   X X   X X   X X X     X             31
```
