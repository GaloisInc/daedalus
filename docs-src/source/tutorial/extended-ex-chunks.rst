Extended Exercise: PNG Chunks
=============================

Now, we'll use our utilities and more of the knowledge we've developed to build
up parsers for PNG chunks themselves, culminating in a top-level parser that
can consume full PNG images. This is a very long section - take it as slow as
you need, and don't worry if you need to peek at the solutions as you go!

The central consideration for us will be the *chunk type* - in PNG, these types
are given by special 4-byte sequences identifying what kind of data is carried
by a chunk. You can't guess these sequences; they're given explicitly in the
PNG specification.

Unfortunately, with our current toolkit, there isn't a nice way to capture
these kinds of alternatives - tagged sums are close, but not sufficient on
their own.

Introducing ``bitdata``
-----------------------

DaeDaLus offers a solution: ``bitdata``. In general, a ``bitdata`` specifies
how bytes should be broken into groups of bits, which are *then* combined into
a tagged sum.

Here is a small example of using ``bitdata`` to inspect the first nybble of a
byte:

.. code-block:: DaeDaLus

    bitdata ChooseOption where
      opt1 = 0x0 : uint 4
      opt2 = 0x1

    bitdata OptionData where
      OptionData = { opt : ChooseOption, val : uint 4 }

    ...

    block
      let odata = UInt8 as? OptionData
      case odat of
        OptionData x ->
          case x.opt of
            opt1 -> ^ x.val
            _    -> Fail "Wrong option!"

Note that we write ``bitdata ... where ...`` to introduce a new bitdata.
Furthermore, note that ``ChooseOption`` and ``OptionData`` are *not* parsers!
A ``bitdata``, in essence, only introduces a type that can be used in
coercions - it doesn't do anything else at all until we use something like
``as?``. Once we've done that, however, we can take advantage of all the tools
available for breaking down tagged sums, such as pattern-matching.

Note also here that, to access the fields of a structure, we can use the ``.``
accessor notation. In the ``bitdata OptionData``, we have only one variant that
has two fields, each of length 4 bits.

The type annotations in this example are necessary, otherwise DaeDaLus would
not know where to place the boundary between ``opt`` and ``val``; there are
lots of ways to split a byte into two chunks!

PNG Chunk Types
---------------

To get your feet wet with ``bitdata``, you'll first define one that specifies
the different type tags that a PNG chunk can carry.

Every type tag is a 32-bit unsigned integer consisting of four bytes. The following
table gives the four bytes for each type. Don't worry about what the tag names
mean; if you're curious, you can check the PNG specification for details, but
we won't need that information for parsing:

.. list-table:: PNG Chunk Types

    * - plte
      - 80 76 84 69
    * - idat
      - 73 68 65 84
    * - trns
      - 116 82 78 83
    * - chrm
      - 99 72 82 77
    * - gama
      - 103 65 77 65
    * - iccp
      - 105 67 67 80
    * - sbit
      - 115 66 73 84
    * - srgb
      - 115 82 71 66
    * - text
      - 116 69 88 116
    * - itxt
      - 105 84 88 116
    * - ztxt
      - 122 84 88 116
    * - bkgd
      - 98 75 71 68
    * - hist
      - 104 73 83 84
    * - phys
      - 112 72 89 115
    * - splt
      - 115 80 76 84
    * - time
      - 116 73 77 69

**Exercise:** Write a ``bitdata`` called ``ChunkType`` with 16 variants (one
for each name in the above table.) Name the variants using the names in the
table, and be sure to specify the type of each byte (use ``uint 8``.)

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 44
        :lines: 44-60

PNG Chunk Data
--------------

Now, you'll build parsers for each type of chunk data. We'll then combine those
into a single ``ChunkData`` parser, which will allow us to parse any type of
chunk with a single parser, and more importantly, parse many chunks in
sequence.

PLTE
^^^^

The PLTE chunk contains between 1 and 256 palette entries, which are simply RGB
structures as we defined in the section
:ref:`extended exercise: defining helpful utilities`.

**Exercise:** Write a parser ``PLTEChunkData`` that parses between 1 and 256
``RGB`` structures.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 81
        :lines: 81

IDAT
^^^^

The IDAT chunk contains image data as output by the PNG compression algorithm;
it consists merely of a bunch of bytes.

**Exercise:** Write a parser ``IDATChunkData`` that parses any number of bytes.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 83
        :lines: 83

tRNS
^^^^

Transparency chunks are significantly more complex than the others we've looked
at so far, in that they have a different data shape depending on data provided
in the PNG *signature* that we have not defined yet. We'll delay full
discussion of that until later; for now, all that matters is that the signature
is a structure with a field named ``colour_type``, which we can patterm-match
on to guide value construction for different PNG modes.

Transparency data is valid for ``colour_type`` values 0, 2, and 3 - any other
value should cause transparency data parsing to fail.

In the case of mode 0, the transparency data is a single big-endian 2-byte
value which we will call ``grey_sample_value``.

In mode 2, the transparency data is a sequence of three big-endian 2-byte
values which we call ``red_sample_value``, ``blue_sample_value``, and
``green_sample_value``. This is the order the values should be parsed.

Finally, in mode 3, there is a sequence of bytes, one for each entry in the
PLTE chunk (so, between 1 and 256.) You are not required to check that there
are an appropriate number of bytes - in fact, you should not put bounds on how
many are parsed. We call these bytes ``alpha_for_palette``.

**Exercise:** Define three parsers, ``TRNSData0``, ``TRNSData2``, and
``TRNSData3``, that carry the data as described above. Use the names provided
as field names (so, make sure these parsers all produce structures.)

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 92
        :lines: 92-104

**Exercise:** Now, define a parser ``TRNSChunkData`` that takes a single
argument, ``sig``, and uses the ``colour_type`` field of that argument to
produce an appropriate value; you can use pattern-matching and integer literals
for this. In all other cases, the parser should fail with a message indicating
that the transparency chunk cannot appear for any other color mode.

.. dropdown:: Hint
    :color: info

    Remember that you can return values of a tagged sum using the special
    'barbed wire' brackets, ``{| tag = ... |}``.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 85
        :lines: 85-90

cHRM
^^^^

The chromaticities / white point chunk is eight big-endian 4-byte values:
``x`` and ``y`` respectively for the white point, red, green, and blue.
You should use the names ``white_point_x``, ``white_point_y``, ``red_x``,
``red_y``, ``green_x``, ``green_y``, ``blue_x``, and ``blue_y`` for these
fields in the next exercise.

**Exercise:** Define a parser ``CHRMChunkData`` that parses the fields
described above.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 106
        :lines: 106-115

gAMA
^^^^

The gamma chunk relates image samples to desired output intensity. It is simply
a big-endian 4-byte integer.

**Exercise:** Define a parser ``GAMAChunkData`` that parses a single field,
``image_gamma``, as specified in the above description.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 117
        :lines: 117-119

iCCP
^^^^

The embedded ICC profile chunk contains information related to the
International Color Consortium. It consists of three fields:

1. A null-terminated string between 1 and 79 characters in length, which we
   call ``profile_name``
2. A byte defining the ``compression_method``
3. Many bytes giving the ``compressed_profile``

**Exercise:** Define a parser ``ICCPChunkData`` that parses a structure with
the three fields described above.

.. dropdown:: Hint
    :color: info

    Remember the null-terminated string parser we built previously!

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 121
        :lines: 121-125

sBIT
^^^^

The significant bits chunk allows lossless data recovery even if the sample
depth isn't supported by PNG. The supported color modes each have a number of
significant bits to store that allows this to be done.

Like the transparency chunk described above, the significant bits chunk behaves
differently depending on the PNG color mode being used.

In mode 0, the significant bits data is a single byte which we call
``significant_greyscale_bits``.

In modes 2 and 3, the data is stored in three bytes, respectively called
``significant_red_bits``, ``significant_green_bits``, and
``significant_blue_bits``.

In mode 4, there are two bytes: ``significant_greyscale_bits`` and
``significant_alpha_bits``.

Finally, in mode 6, there are four bytes: ``significant_red_bits``,
``significant_green_bits``, ``significant_blue_bits``, and
``significant_alpha_bits``.

**Exercise:** Define four parsers, ``SBITData0``, ``SBITData2or3``,
``SBITData4``, and ``SBITData6``, that carry the data as described above. Name
the fields using the names we provided (so make sure all parsers return
structures).

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 135
        :lines: 135-155

**Exercise:** Now, define a parser ``SBITChunkData`` that takes a single
argument, ``sig``, and uses the ``colour_type`` field of that argument to
produce an appropriate value; you can use pattern-matching and integer literals
for this. You can ignore all other color modes.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 127
        :lines: 127-133

sRGB
^^^^

The standard RGB color space chunk defines a *rendering intent*, defined by the
ICC. It contains one byte, which is a value between 0 and 3 (inclusive) giving
the intent.

**Exercise:** Define a parser, ``SRGBChunkData``, that parses a structure with
a single field, ``rendering_intent``, from exactly one byte with a value
between 0 and 3 (inclusive).

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 157
        :lines: 157-159

tEXt
^^^^

PNG provides textual data blocks to store information associated with images,
such as descriptions and copyright notices. There are three types of textual
chunk, the most basic of which is the tEXt chunk we'll define first.

The standard tEXt chunk is comprised of two pieces of data: a null-terminated
string giving a keyword (some of which are recognized by the PNG specification)
and zero or more bytes giving the text associated with the keyword. Note that
this character string (the data following the keyword) is *not*
null-terminated.

**Exercise:** Define a parser ``TEXTChunkData`` that returns a structure with
two fields, ``keyword`` and ``text_string``. ``keyword`` should be a
null-terminated string between 1 and 79 characters in length, and
``text_string`` should be an arbitrarily large number of bytes.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 161
        :lines: 161-164

zTXt
^^^^

The zTXt chunk is semantically the same as tEXt, but with compressed data - it
is recommended for use with large blocks of text.

**Exercise:** Define a parser ``ZTXTChunkData`` that returns a structure with
three fields: ``keyword``, ``compression_method``, and
``compressed_text_datastream``. The first and last of these are exactly as they
were for ``TEXTChunkData``, and ``compression_method`` is a single byte.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 166
        :lines: 166-170

iTXt
^^^^

The final type of textual data chunk is iTXt, which is used for international
text. It consists of the following fields:

* ``keyword``: Exactly the same as the previous two chunk types
* ``compression_flag``: A ``FLAG`` indicating whether the text data is
  compressed
* ``compression_method``: The same as in zTXt
* ``language_tag``: An arbitrarily long null-terminated string indicating the
  language used for the text
* ``translated_keyword``: An arbitrarily long null-terminated string giving the
  translation of the keyword into the language of the text
* ``text``: The text data, given in the same way as both tEXt and zTXt

**Exercise:** Define a parser ``ITXTChunkData`` that returns a structure with
the six fields as described above.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 172
        :lines: 172-179

bKGD
^^^^

The background color chunk specifies the default background color to present
the image against. Different color modes use different data in this chunk to
achieve the specified effects.

For color modes 0 and 4, a single two-byte value ``greyscale`` controls the
background color.

For modes 2 and 6, three two-byte values, ``red``, ``green``, and ``blue`` are
used for the background color.

Finally, for color mode 3, a single byte, ``palette_index``, is used. You do
not need to check that this index is within range for the palette provided.

**Exercise:** Define three parsers, ``BKGDData0or4``, ``BKGDData2or6``, and
``BKGDData3`` that return structures with the fields described above.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 189
        :lines: 189-201

**Exercise:** Now, define a parser ``BKGDChunkData`` that takes a single
argument, ``sig``, and uses the ``colour_type`` field of that argument to
prouce an appropriate value; you can use pattern-matching and integer literals
for this. You can ignore all other color modes.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 181
        :lines: 181-187

hIST
^^^^

The image histogram chunk gives approximate usage frequencies for all colors in
the palette. Each frequency is a big-endian two-byte integer.

**Exercise:** Define a parser ``HISTChunkData`` that returns a structure with a
single field, ``frequencies``, which is an array of frequencies as described
above. Don't worry about checking that the correct number of frequencies are
provided.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 203
        :lines: 203-205

pHYs
^^^^

The physical pixel dimensions chunk describes the intended aspect ratio for
image display. It's comprised of:

* ``pixels_per_unit_x_axis``, a 4-byte unsigned integer
* ``pixels_per_unit_y_axis``, another 4-byte unsigned integer
* ``unit_specifier``, a ``FLAG`` deciding whether to use unitless aspect ratio
  or specify actual size

**Exercise:** Define a parser ``PHYSChunkData`` that returns a structure with
the fields as described above.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 207
        :lines: 207-211

sPLT
^^^^

The suggested palette chunk is fairly complex semantically - if you want to
know the gnarly details, we once again point you to the full PNG specification
linked earlier in this section. As it turns out, it is also likely the most
challenging exercise on this page!

The chunk consists of the following data:

* ``palette_name``, a null-terminated string between 1 and 79 characters in
  length
* ``sample_depth``, a single byte that *must* be either 8 or 16
* ``palette``, a series of 6 or 10 byte structures depending on the sample
  depth. In both cases, the data are:

  * ``red``
  * ``green``
  * ``blue``
  * ``alpha``
  * ``frequency``

  In all cases, the ``frequency`` is given by a 2-byte unsigned integer.
  If the sample depth is 8, the other four samples are one byte; if it is
  16, they are two bytes (this is where we get a total of 6 or 10, since
  the ``frequency`` is always given in two bytes.)

Put in higher-level terms: We now need to define a data-dependent parser, as
the sample depth will control how we parse the remaining bytes in hte input.

Let's build this up one small step at a time.

**Exercise:** Define two parsers, ``SPLTSample8`` and ``SPLTSample16``, that
parse the five sample values as described above.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 224
        :lines: 224-238

**Exercise:** Using the two parsers you just defined, define a new parser
``SPLTSample`` that takes a single argument, ``depth : uint 8``, and decides
which to use. The result should be a tagged sum with two variants named
``splt_sample_depth_8`` and ``splt_sample_depth_16``. You do not need to write
anything to handle sample depths other than 8 and 16.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 219
        :lines: 219-222

**Exercise (Challenging):** Finally, define a parser ``SPLTChunkData`` that
returns a structure containing fields ``palette_name``, ``sample_depth``, and
``palette`` as described earlier.

.. dropdown:: Hint
    :color: info

    Remember that you can parse alternatives using the operators ``<|`` and
    ``|`` - the former is for biased choice, the latter for unbiased.

    Also remember that DaeDaLus allows for data-dependent parsing: If you store
    the result of running a parser in a local variable or structure field, you
    may use that value later in the sequence.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 213
        :lines: 213-217

tIME
^^^^

With that tricky one out of the way, there's only one chunk type left, and it's
very straightforward: The tIME chunk carries along the last-modified time for
the image.

**Exercise:** For consistency with our other chunk parser naming, define a
'new' parser ``TIMEChunkData`` that returns a ``UTCTime``.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 240
        :lines: 240

Generic Chunk Data Parsing
^^^^^^^^^^^^^^^^^^^^^^^^^^

Now that we know how to parse each type of chunk, we want to combine all of
those possibilities into a single tagged sum type, so that we will be able to
parse many chunks in sequence. To do this, we'll use pattern-matching on the
``bitdata`` we defined earlier and all of the parsers defined so far in this
section.

**Exercise:** Write a parser ``ChunkData`` that takes two arguments, ``sig``
and ``type : ChunkType``, and produces a tagged sum covering all possible
chunk types defined by the ``type`` parameter. The tags should be named,
for example, ``plte_data`` for the ``plte`` case.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 62
        :lines: 62-79

PNG Chunks
----------

Now that we can successfully parse any type of chunk data, we need only add in
the metadata necessary for a complete chunk. A complete PNG chunk consists of
the chunks length (as a 4-byte integer), the type of the chunk (which we need
to interpret as a ``ChunkType``), the chunk data, and a CRC value.

**Exercise:** Define a parser ``PNGChunk`` that takes a single argument,
``sig``, and returns a structure with the following fields:

* ``type``, an instance of ``bitdata ChunkType``
* ``data``, a ``ChunkData`` that results from parsing exactly as many bytes as
  specified by the length (which must be parsed before ``type`` but does not
  need to be stored in the resulting structure)
* ``crc``, a ``Crc``

.. dropdown:: Hint
    :color: info

    1. Remember that you use a ``bitdata`` by coercing a parsed value - take
       care to use the appropriate coercion method!
    2. To make sure a particular number of bytes are consumed, recall the
       ``Chunk`` parser in the standard library. Be careful - this expects a
       ``uint 64`` argument for the number of bytes to parse!

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 242
        :lines: 242-247

Header / Trailer
----------------

Two special chunks we haven't discussed are the *header* and *trailer* for PNG.
These are, respectively, the first and last chunks in a PNG datastream. The
``sig`` parameter we've left undiscussed is, in fact, the data in the header.

Like the ``PNGChunk`` variants, these will both need a length, type, and crc
field. Since they always have the same length (13 and 0 bytes respectively), we
can hardcode these fields in our parsers. Note that the length, type, and crc
fields do not count towards the length of the chunk.

The header chunk additionally contains the following fields:

* ``width``, a 4-byte unsigned integer giving the width of the image
* ``height``, a 4-byte unsigned integer giving the height of the image
* ``bit_depth``, a single byte giving the number of bits per sample
* ``colour_type``, a single byte that defines the image type. This must be
  one of 0, 2, 3, 4, or 6
* ``compression_method``, a single byte giving the compression method
* ``filter_method``, a single byte indicating the preprocessing method applied
  before compression
* ``interlace_method``, a single byte indicating transmission order of image
  data

**Exercise:** Define a parser ``IHDRChunk`` that returns a structure with the
above-described fields. The type identifier for the header chunk is the four
bytes ``73``, ``72``, ``68``, and ``82``.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 249
        :lines: 249-260

**Exercise:** Define a parser ``IENDChunk`` that returns a structure matching
the description above. The type identifier for the trailer chunk is the four
bytes ``73``, ``69``, ``78``, and ``68``.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 262
        :lines: 262-266

Full PNG
--------

The home stretch! We have almost all of the components needed to parse full PNG
images now. The only thing missing is the PNG header, a byte sequence that
starts every PNG image ever encoded:

.. literalinclude:: ../examples/png.ddl
    :language: DaeDaLus
    :lineno-start: 268
    :lines: 268-269

With this here is your final exercise:

**Exercise:** Write the ``Main`` parser to parse a full PNG image. That is:
The PNG header, the signature/header chunk, the data chunks, and finally the
trailer chunk. Make sure ``Main`` is defined so that it only succeeds if it
consumes the entire input.

.. dropdown:: Solution
    :color: warning

    .. literalinclude:: ../examples/png.ddl
        :language: DaeDaLus
        :lineno-start: 271
        :lines: 271-277

Conclusion
----------

Congratulations on making it through! The full PNG specification can be foud on
the following page; you'll be pleasantly surprised at how short it is compared
to the PNG specification it's based on.

Note that in a few places (as the exercises note), we fail to do some of the
validation included in the specifciation; in fact, there are a number of places
where we purposefully left out the restrictions for simplicity (e.g. the
``bit_depth`` field only has a few allowed values, and these values are also
constrained by the ``colour_type``.) As discussed, it is often better to leave
these more complex validations for post-parsing stages of your applications,
such as type-checking and other static analysis. Where it was natural, we built
the specification's restrictions into our parser to catch problems early.

You're encouraged to read over the rest of the DaeDaLus user guide, which has
some extra detail on concepts we covered (and some more advanced topics we did
not cover.)
