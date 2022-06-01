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


