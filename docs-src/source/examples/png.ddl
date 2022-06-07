import Daedalus

-- Utilities

def Length = BEUInt32
def Crc = BEUInt32

def FLAG = $[0 .. 1]

def NullChar    = $[0]
def NonNullChar = $[1 .. 255]

def OMany (omin:maybe (uint 64)) (omax:maybe (uint 64)) P =
  case omin of
    nothing  -> case omax of
                  nothing  -> Many P
                  just max -> Many (..max) P
    just min -> case omax of
                  nothing  -> Many (min..) P
                  just max -> Many (min..max) P

def NTString (omin:maybe (uint 64)) (omax:maybe (uint 64)) =
  block
    $$ = OMany omin omax NonNullChar
    NullChar

def RGB =
  block
    red = UInt8
    green = UInt8
    blue = UInt8

def UTCTime =
  block
    year = BEUInt16
    month = $[1 .. 12]
    day = $[1 .. 31]
    hour = $[0 .. 23]
    minute = $[0 .. 59]
    second = $[0 .. 60]

-- Chunks / PNG

bitdata ChunkType where
  plte = {  80 : uint 8 ;  76 : uint 8 ;  84 : uint 8 ;  69 : uint 8 }
  idat = {  73 : uint 8 ;  68 : uint 8 ;  65 : uint 8 ;  84 : uint 8 }
  trns = { 116 : uint 8 ;  82 : uint 8 ;  78 : uint 8 ;  83 : uint 8 }
  chrm = {  99 : uint 8 ;  72 : uint 8 ;  82 : uint 8 ;  77 : uint 8 }
  gama = { 103 : uint 8 ;  65 : uint 8 ;  77 : uint 8 ;  65 : uint 8 }
  iccp = { 105 : uint 8 ;  67 : uint 8 ;  67 : uint 8 ;  80 : uint 8 }
  sbit = { 115 : uint 8 ;  66 : uint 8 ;  73 : uint 8 ;  84 : uint 8 }
  srgb = { 115 : uint 8 ;  82 : uint 8 ;  71 : uint 8 ;  66 : uint 8 }
  text = { 116 : uint 8 ;  69 : uint 8 ;  88 : uint 8 ; 116 : uint 8 }
  itxt = { 105 : uint 8 ;  84 : uint 8 ;  88 : uint 8 ; 116 : uint 8 }
  ztxt = { 122 : uint 8 ;  84 : uint 8 ;  88 : uint 8 ; 116 : uint 8 }
  bkgd = {  98 : uint 8 ;  75 : uint 8 ;  71 : uint 8 ;  68 : uint 8 }
  hist = { 104 : uint 8 ;  73 : uint 8 ;  83 : uint 8 ;  84 : uint 8 }
  phys = { 112 : uint 8 ;  72 : uint 8 ;  89 : uint 8 ; 115 : uint 8 }
  splt = { 115 : uint 8 ;  80 : uint 8 ;  76 : uint 8 ;  84 : uint 8 }
  time = { 116 : uint 8 ;  73 : uint 8 ;  77 : uint 8 ;  69 : uint 8 }

def ChunkData sig (type : ChunkType) =
  case type of
    plte -> {| plte_data = PLTEChunkData     |}
    idat -> {| idat_data = IDATChunkData     |}
    trns -> {| trns_data = TRNSChunkData sig |}
    chrm -> {| chrm_data = CHRMChunkData     |}
    gama -> {| gama_data = GAMAChunkData     |}
    iccp -> {| iccp_data = ICCPChunkData     |}
    sbit -> {| sbit_data = SBITChunkData sig |}
    srgb -> {| srgb_data = SRGBChunkData     |}
    text -> {| text_data = TEXTChunkData     |}
    itxt -> {| itxt_data = ITXTChunkData     |}
    ztxt -> {| ztxt_data = ZTXTChunkData     |}
    bkgd -> {| bkgd_data = BKGDChunkData sig |}
    hist -> {| hist_data = HISTChunkData     |}
    phys -> {| phys_data = PHYSChunkData     |}
    splt -> {| splt_data = SPLTChunkData     |}
    time -> {| time_data = TIMEChunkData     |}

def PLTEChunkData = Many (1..256) RGB

def IDATChunkData = Many UInt8

def TRNSChunkData sig =
  case sig.colour_type of
    0 -> {| trns_colour_type_0 = TRNSData0 |}
    2 -> {| trns_colour_type_2 = TRNSData2 |}
    3 -> {| trns_colour_type_3 = TRNSData3 |}
    _ -> Fail "tRNS chunk shall not appear for other colour types"

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

def CHRMChunkData =
  block
    white_point_x = BEUInt32
    white_point_y = BEUInt32
    red_x         = BEUInt32
    red_y         = BEUInt32
    green_x       = BEUInt32
    green_y       = BEUInt32
    blue_x        = BEUInt32
    blue_y        = BEUInt32

def GAMAChunkData =
  block
    image_gamma = BEUInt32

def ICCPChunkData =
  block
    profile_name       = NTString (just 1) (just 79)
    compression_method = UInt8
    compressed_profile = Many UInt8

def SBITChunkData sig =
    case sig.colour_type of
      0 -> {| sbit_colour_type_0 = SBITData0 |}
      2 -> {| sbit_colour_type_2 = SBITData2or3 |}
      3 -> {| sbit_colour_type_3 = SBITData2or3 |}
      4 -> {| sbit_colour_type_4 = SBITData4 |}
      6 -> {| sbit_colour_type_6 = SBITData6 |}

def SBITData0 =
  block
    significant_greyscale_bits = UInt8

def SBITData2or3 =
  block
    significant_red_bits   = UInt8
    significant_green_bits = UInt8
    significant_blue_bits  = UInt8

def SBITData4 =
  block
    significant_greyscale_bits = UInt8
    significant_alpha_bits     = UInt8

def SBITData6 =
  block
    significant_red_bits   = UInt8
    significant_green_bits = UInt8
    significant_blue_bits  = UInt8
    significant_alpha_bits = UInt8

def SRGBChunkData =
  block
    rendering_intent = $[0 .. 3]

def TEXTChunkData =
  block
    keyword = NTString (just 1) (just 79)
    text_string = Many UInt8

def ZTXTChunkData =
  block
    keyword = NTString (just 1) (just 79)
    compression_method = UInt8
    compressed_text_datastream = Many UInt8

def ITXTChunkData =
  block
    keyword = NTString (just 1) (just 79)
    compression_flag = FLAG
    compression_method = UInt8
    language_tag = NTString nothing nothing
    translated_keyword = NTString nothing nothing
    text = Many UInt8

def BKGDChunkData sig =
  case sig.colour_type of
    0 -> {| bkgd_colour_type_0 = BKGDData0or4 |}
    4 -> {| bkgd_colour_type_4 = BKGDData0or4 |}
    2 -> {| bkgd_colour_type_2 = BKGDData2or6 |}
    6 -> {| bkgd_colour_type_6 = BKGDData2or6 |}
    3 -> {| bkgd_colour_type_3 = BKGDData3 |}

def BKGDData0or4 =
  block
    greyscale = BEUInt16

def BKGDData2or6 =
  block
    red   = BEUInt16
    green = BEUInt16
    blue  = BEUInt16

def BKGDData3 =
  block
    palette_index = UInt8

def HISTChunkData =
  block
    frequencies = Many BEUInt16

def PHYSChunkData =
  block
    pixels_per_unit_x_axis = BEUInt32
    pixels_per_unit_y_axis = BEUInt32
    unit_specifier         = FLAG

def SPLTChunkData =
  block
    palette_name = NTString (just 1) (just 79)
    sample_depth = $[ 8 ] <| $[ 16 ]
    Many (SPLTSample sample_depth)

def SPLTSample (depth : uint 8) =
  case depth of
    8  -> {| splt_sample_depth_8  = SPLTSample8 |}
    16 -> {| splt_sample_depth_16 = SPLTSample16 |}

def SPLTSample8 =
  block
    red       = UInt8
    green     = UInt8
    blue      = UInt8
    alpha     = UInt8
    frequency = BEUInt16

def SPLTSample16 =
  block
    red       = BEUInt16
    green     = BEUInt16
    blue      = BEUInt16
    alpha     = BEUInt16
    frequency = BEUInt16

def TIMEChunkData = UTCTime

def PNGChunk sig =
  block
    let len = Length as uint 64
    type = BEUInt32 as? ChunkType
    data = Chunk len (ChunkData sig type)
    crc = Crc

def IHDRChunk =
  block
    Match [ 0; 0; 0; 13]
    Match [ 73; 72; 68; 82]
    width              = BEUInt32
    height             = BEUInt32
    bit_depth          = UInt8
    colour_type        = UInt8
    compression_method = UInt8
    filter_method      = UInt8
    interlace_method   = UInt8
    crc                = Crc

def IENDChunk =
  block
    Match [0; 0; 0; 0]
    Match [73; 69; 78; 68]
    crc = Crc

def PNGHeader =
  Match [ 137; 80; 78; 71; 13; 10; 26; 10]

def Main =
  block
    PNGHeader
    signature = IHDRChunk
    chunks = Many (PNGChunk signature)
    IENDChunk
    END
