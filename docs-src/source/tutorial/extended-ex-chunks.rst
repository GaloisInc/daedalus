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

    .. code-block:: DaeDaLus

        bitdata ChunkType where
          plte = {  80 : uint 8,  76 : uint 8,  84 : uint 8,  69 : uint 8 }
          idat = {  73 : uint 8,  68 : uint 8,  65 : uint 8,  84 : uint 8 }
          trns = { 116 : uint 8,  82 : uint 8,  78 : uint 8,  83 : uint 8 }
          chrm = {  99 : uint 8,  72 : uint 8,  82 : uint 8,  77 : uint 8 }
          gama = { 103 : uint 8,  65 : uint 8,  77 : uint 8,  65 : uint 8 }
          iccp = { 105 : uint 8,  67 : uint 8,  67 : uint 8,  80 : uint 8 }
          sbit = { 115 : uint 8,  66 : uint 8,  73 : uint 8,  84 : uint 8 }
          srgb = { 115 : uint 8,  82 : uint 8,  71 : uint 8,  66 : uint 8 }
          text = { 116 : uint 8,  69 : uint 8,  88 : uint 8, 116 : uint 8 }
          itxt = { 105 : uint 8,  84 : uint 8,  88 : uint 8, 116 : uint 8 }
          ztxt = { 122 : uint 8,  84 : uint 8,  88 : uint 8, 116 : uint 8 }
          bkgd = {  98 : uint 8,  75 : uint 8,  71 : uint 8,  68 : uint 8 }
          hist = { 104 : uint 8,  73 : uint 8,  83 : uint 8,  84 : uint 8 }
          phys = { 112 : uint 8,  72 : uint 8,  89 : uint 8, 115 : uint 8 }
          splt = { 115 : uint 8,  80 : uint 8,  76 : uint 8,  84 : uint 8 }
          time = { 116 : uint 8,  73 : uint 8,  77 : uint 8,  69 : uint 8 }

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

    .. code-block:: DaeDaLus

        def PLTEChunkData = Many (1..256) RGB

IDAT
^^^^

The IDAT chunk contains image data as output by the PNG compression algorithm;
it consists merely of a bunch of bytes.

**Exercise:** Write a parser ``IDATChunkData`` that parses any number of bytes.

.. dropdown:: Solution
    :color: warning

    .. code-block:: DaeDaLus

        def IDATChunkData = Many UInt8

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

    def TRNSData0 =
      block
        grey_sample_value = BEUInt16

    def TRNSData2 =
      block
        red_sample_value   = BEUInt16
        blue_sample_value  = BEUInt16
        green_sample_value = BEUInt16

    def TRNSData3 =
      block
        alpha_for_palette = Many UInt8

**Exercise:** Now, define a parser ``TRNSChunkData`` that takes a single
argument, sig, and uses the ``colour_type`` field of that argument to produce
an appropriate value; you can use pattern-matching and integer literals for
this. In all other cases, the parser should fail with a message indicating that
the transparency chunk cannot appear for any other color mode.

.. dropdown:: Hint
    :color: info

    Remember that you can return values of a tagged sum using the special
    'barbed wire' brackets, ``{| tag = ... |}``.

.. dropdown:: Solution
    :color: warning

    .. code-block:: DaeDaLus

        def TRNSChunkData sig =
          case sig.colour_type of
            0 -> {| trns_colour_type_0 = TRNSData0 |}
            2 -> {| trns_colour_type_2 = TRNSData2 |}
            3 -> {| trns_colour_type_3 = TRNSData3 |}
            _ -> Fail "tRNS chunk shall not appear for other colour types"

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

    .. code-block:: DaeDaLus

        def CHRMChunkData sig =
          block
            white_point_x = BEUInt32
            white_point_y = BEUInt32
            red_x         = BEUInt32
            red_y         = BEUInt32
            green_x       = BEUInt32
            green_y       = BEUInt32
            blue_x        = BEUInt32
            blue_y        = BEUInt32

gAMA
^^^^

The gamma chunk relates image samples to desired output intensity. It is simply
a big-endian 4-byte integer.

**Exercise:** Define a parser ``GAMAChunkData`` that parses a single field,
``image_gamma``, as specified in the above description.

.. dropdown:: Solution
    :color: warning

    .. code-block:: DaeDaLus

      def GAMAChunkData =
        block
          image_gamma = BEUInt32

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

    .. code-block:: DaeDaLus

        def ICCPChunkData =
          block
            profile_name       = NTString (just 1) (just 79)
            compression_method = UInt8
            compressed_profile = Many UInt8


