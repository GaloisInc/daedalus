import Daedalus

-- ENTRY
def Main =
  block
    let s = GetStream
    profileHeader = ProfileHeader
    map (entry in TagTable) (ParseTagIn s entry)


--------------------------------------------------------------------------------
-- Header (Section 7.2)

def ProfileHeader =
  block
    size                = BE32
    preferred_cmm_type  = Many 4 UInt8
    version             = VersionField
    devce_class         = ProfileClass
    color_space         = DataColorSpace
    pcs                 = DataColorSpace  -- check additional constraints?
    creation_date_time  = DateTimeNumber
    Match "acsp"
    primary_platform    = PrimaryPlatform
    profile_flags       = BE32            -- XXX: bitdata, see 7.2.13
    device_manufacturer = Many 4 UInt8
    device_model        = Many 4 UInt8
    device_attributes   = BE64            -- XXX: bitdata, see 7.2.16
    rendering_intent    = RenderingIntent
    illuminant          = XYZNumber
    creatior            = Many 4 UInt8
    identifier          = Many 16 UInt8
    reserved_data       = Many 28 UInt8      -- XXX: spectral pcc

def VersionField =
  block
    major        = UInt8
    let min_bf   = UInt8
    minor        = min_bf >> 4 as! uint 4
    bugfix       = min_bf      as! uint 4
    Match [0x00, 0x00]

def ProfileClass =
  case BE32 of
    0s"scnr" -> {| input_device_profile   |}
    0s"mntr" -> {| display_device_profile |}
    0s"prtr" -> {| output_device_profile  |}
    0s"link" -> {| device_link_profile    |}
    0s"spac" -> {| color_space_profile    |}
    0s"abst" -> {| abstract_profile       |}
    0s"nmcl" -> {| named_color_profile    |}
    0s"cenc" -> {| color_encoding_space   |}
    0s"mid " -> {| multiplex_id           |}
    0s"mlnk" -> {| multiplex_link         |}
    0s"mvis" -> {| multiplex_vis          |}


def DataColorSpace =
  block
    let tag = BE32
    case tag of
      0s"XYZ " -> {| nciexyz_or_pcsxyz |}
      0s"Lab " -> {| cielab_or_pcslab  |}
      0s"Luv " -> {| cieluv            |}
      0s"Ycbr" -> {| ycbcr             |}
      0s"Yxy " -> {| cieyxy            |}
      0s"LMS " -> {| lms               |}
      0s"RGB " -> {| rgb               |}
      0s"GRAY" -> {| gray              |}
      0s"HSV " -> {| hsv               |}
      0s"HLS " -> {| hls               |}
      0s"CMYK" -> {| cmyk              |}
      0s"CMY " -> {| cmy               |}
      0s"2CLR" -> {| two_colour        |}
      0s"3CLR" -> {| three_colour      |}
      0s"4CLR" -> {| four_colour       |}
      0s"5CLR" -> {| five_colour       |}
      0s"6CLR" -> {| six_colour        |}
      0s"7CLR" -> {| seven_colour      |}
      0s"8CLR" -> {| eight_colour      |}
      0s"9CLR" -> {| nine_colour       |}
      0s"ACLR" -> {| ten_colour        |}
      0s"BCLR" -> {| eleven_colour     |}
      0s"CCLR" -> {| twelve_colour     |}
      0s"DCLR" -> {| thirteen_colour   |}
      0s"ECLR" -> {| fourteen_colour   |}
      0s"FCLR" -> {| fifteen_colour    |}
      0        -> {| none |}
      _        -> block
                    Guard ((tag >> 16) == 0s"ne")
                    {| n_channel = tag as! uint 16 |}


def PrimaryPlatform =
  case BE32 of
    0s"APPL" -> {| apple_computer_inc    |}
    0s"MSFT" -> {| microsoft_corporation |}
    0s"SGI " -> {| silicon_graphics_inc  |}
    0s"SUNW" -> {| sun_microsystems      |}
    0        -> {| none                  |}


def RenderingIntent =
  case BE32 of
    0 -> {| perceptual |}
    1 -> {| media_relative_colorimetric |}
    2 -> {| saturation |}
    3 -> {| icc_absolute_colorimetric |}


--------------------------------------------------------------------------------
-- Tag table (Section 7.3)

def TagTable =
  block
    let n = BE32 as uint 64
    Many n TagEntry

def TagEntry =
  block
    tag_signature           = BE32
    offset_to_data_element  = BE32 as uint 64
    size_of_data_element    = BE32 as uint 64



--------------------------------------------------------------------------------
-- Various kinds of "numbers"  (Section 4)

def XYZNumber =
  block
    x = BE32
    y = BE32
    z = BE32

def XYNumber =
  block
    x = BE32
    y = BE32

def DateTimeNumber =
  block
    year    = BE16
    month   = BE16
    day     = BE16
    hour    = BE16
    minute  = BE16
    second  = BE16

def PositionNumber =
  block
    offset = BE32
    size   = BE32

-- 0 terminated ASCII 7 string (sem value does not include the 0)
def ASCII7 =
  block
    $$ = Many ($[1..] as? uint 7)
    Many (1 ..) $[0] <| Fail "Non 0 string terminator"

def Response16Number =
  block
    device = BE16
    Match [0,0]
    measurement = BE32



def ParseTagIn s (t : TagEntry) =
  block
    SetStreamAt t.offset_to_data_element s
    Chunk t.size_of_data_element (Tag t.tag_signature);




-- ENTRY: Assumes that the offsets are relative to the current stream.
def ParseTag (t : TagEntry) =
  block
    Skip (t.offset_to_data_element as uint 64);
    Chunk (t.size_of_data_element as uint 64) (Tag t.tag_signature);



--------------------------------------------------------------------------------
-- Tag Definitions (Section 9)

def Tag (sig : uint 32) =
  case sig of
    0s"A2B0" -> {| A2B0 = LutAB_or_multi |}
    0s"A2B1" -> {| A2B1 = LutAB_or_multi |}
    0s"A2B2" -> {| A2B2 = LutAB_or_multi |}
    0s"A2B3" -> {| A2B3 = LutAB_or_multi |}
    0s"A2M0" -> {| A2Mo = MultiProcessElementsType |}

    0s"bXYZ" -> {| bXYZ = XYZType |}
    0s"bTRC" -> {| bTRC = SomeCurve |}
    0s"B2A0" -> {| B2A0 = Lut_8_16_BA |}
    0s"B2A1" -> {| B2A1 = Lut_8_16_BA |}
    0s"B2A2" -> {| B2A2 = Lut_8_16_BA |}
    0s"B2D0" -> {| B2D0 = MultiProcessElementsType |}
    0s"B2D1" -> {| B2D1 = MultiProcessElementsType |}
    0s"B2D2" -> {| B2D2 = MultiProcessElementsType |}
    0s"B2D3" -> {| B2D3 = MultiProcessElementsType |}
    0s"calt" -> {| calt = DateTimeType |}
    0s"targ" -> {| targ = TextType |}
    0s"chad" -> {| chad = S15Fixed16ArrayType |}
    0s"clro" -> {| clro = ColorantOrderType |}
    0s"clrt" -> {| clrt = ColorantTableType |}
    0s"clot" -> {| clot = ColorantTableType |}
    0s"ciis" -> {| ciis = SignatureType |}
    0s"cprt" -> {| cprt = MultiLocalizedUnicodeType |}
    0s"dmnd" -> {| dmnd = MultiLocalizedUnicodeType |}
    0s"dmdd" -> {| dmdd = MultiLocalizedUnicodeType |}
    0s"D2B0" -> {| D2B0 = MultiProcessElementsType |}
    0s"D2B1" -> {| D2B1 = MultiProcessElementsType |}
    0s"D2B2" -> {| D2B2 = MultiProcessElementsType |}
    0s"D2B3" -> {| D2B3 = MultiProcessElementsType |}
    0s"gamt" -> {| gamt = Lut_8_16_BA |}
    0s"kTRC" -> {| kTRC = SomeCurve |}
    0s"gXYZ" -> {| gXYZ = XYZType |}
    0s"gTRC" -> {| gTRC = SomeCurve |}
    0s"lumi" -> {| lumi = XYZType |}
    0s"meas" -> {| meas = MeasurementType |}
    0s"wtpt" -> {| wtpt = XYZType |}
    0s"ncl2" -> {| ncl2 = NamedColor2Type |}
    0s"resp" -> {| resp = ResponseCurveSet16Type |}
    0s"rig0" -> {| rig0 = SignatureType |}
    0s"pre0" -> {| pre0 = Lut_8_16_AB_BA |}
    0s"pre1" -> {| pre1 = Lut_8_16_BA |}
    0s"pre2" -> {| pre2 = Lut_8_16_BA |}
    0s"desc" -> {| desc = MultiLocalizedUnicodeType |}
    0s"pseq" -> {| pseq = ProfileSequenceDescType |} -- XXX
    0s"psid" -> {| psid = {} |} -- XXX
    0s"rXYZ" -> {| rXYZ = XYZType |}
    0s"rTRC" -> {| rTRC = SomeCurve |}
    0s"rig2" -> {| rig2 = SignatureType |}
    0s"tech" -> {| tech = SignatureType |}
    0s"vued" -> {| vued = MultiLocalizedUnicodeType |}
    0s"view" -> {| view = ViewConditionsType |}
    _        -> {| unknown = sig |}

def LutAB_or_multi =
  First
    lutAB = LutAToBType
    mpe   = MultiProcessElementsType

def Lut_8_16_AB_BA =
  First
    lut8  = Lut8Type
    lut16 = Lut16Type
    lutAB = LutAToBType
    lutBA = LutBToAType

def Lut_8_16_AB =
  First
    lut8  = Lut8Type
    lut16 = Lut16Type
    lutAB = LutAToBType

def Lut_8_16_BA =
  First
    lut8  = Lut8Type
    lut16 = Lut16Type
    lutBA = LutBToAType

def SomeCurve =
  First
    curve            = CurveType
    parametric_curve = ParametricCurveType



--------------------------------------------------------------------------------
-- Tag types (Section 10)

def StartTag x =
  block
    Match x
    Match [0,0,0,0]


def DateTimeType =
  block
    StartTag "dtim"
    DateTimeNumber

def TextType =
  block
    StartTag "text"
    Only ASCII7

def SignatureType =
  block
    StartTag "sig "
    Many 4 UInt8

def MultiLocalizedUnicodeType =
  block
    let s = GetStream   -- Offsets are relative to here
    StartTag "mluc"
    let record_number = BE32
    let record_size   = BE32
    Guard (record_size == 12)
    Many (record_number as uint 64) (UnicodeRecord s)

def UnicodeRecord s =
  block
    language    = BE16
    country     = BE16
    let size    = BE32 as uint 64
    let offset  = BE32 as uint 64
    data        = LookAhead (ChunkRelativeTo s offset size)

def S15Fixed16ArrayType =
  block
    StartTag "sf32"
    Many BE32     -- fixed point rationals

def ChromaticityType =
  block
    StartTag "chrm"
    let number_of_device_channels = BE16 as uint 64
    phosphor_or_colorant          = BE16
    cie_coords                    = Many number_of_device_channels XYNumber

def ColorantOrderType =
  block
    StartTag "clro"
    let count_of_colorants = BE32
    Many UInt8;

def ColorantTableType =
  block
    StartTag "clrt"
    let count_of_colorant = BE32 as uint 64
    Many count_of_colorant Colorant

def Colorant =
  block
    name = Chunk 32 (Only ASCII7)
    pcs  = Many 3 BE16

def CurveType =
  block
    StartTag "curv"
    let n = BE32 as uint 64
    Many n BE16

def ParametricCurveType =
  block
    StartTag "para"
    function = BE16
    Match [0,0]
    parameters = Many BE32
    -- These are to be interpreted as fixed precision rationals

def ResponseCurveSet16Type =
  block
    let s = GetStream
    StartTag "rcs2"
    let number_of_channels = BE16 as uint 64
    let count              = BE16 as uint 64
    Many count
      block
        let off = BE32 as uint 64
        LookAhead
          block
            SetStreamAt off s
            ResponseCurve (number_of_channels)

def ResponseCurve n =
  block
    measurement_unit  = BE32
    let counts        = Many n BE32
    pcxyzs            = Many n XYNumber
    response_arrays   =
      map (qi in counts) (Many (qi as uint 64) Response16Number)

def Lut8Type =
  block
    StartTag "mft1"
    number_of_input_channels = UInt8
    let i = number_of_input_channels as uint 64
    number_of_output_channels = UInt8
    let o = number_of_output_channels as uint 64
    number_of_clut_grid_points = UInt8
    let g = number_of_clut_grid_points as uint 64
    $[ 0x00 ]
    encoded_e_parameters = Many 9 (BE32 as! sint 32)
    input_tables         = Bytes (256 * i)
    clut_values          = Bytes (exp g i * o)
    output_tables        = Bytes (256 * o)


def Lut16Type =
  block
    StartTag "mft2"
    number_of_input_channels = UInt8
    let i = number_of_input_channels as uint 64
    number_of_output_channels = UInt8
    let o = number_of_output_channels as uint 64
    number_of_clut_grid_points = UInt8
    let g = number_of_clut_grid_points as uint 64
    Match1 0x00
    encoded_e_parameters = Many 9 (BE32 as! sint 32)
    number_of_input_table_entries = BE32
    let n = number_of_input_table_entries as uint 64
    number_of_output_table_entries = BE32
    let m = number_of_output_table_entries as uint 64
    input_tables  = Bytes (256 * n * i)
    clut_values   = Bytes (2 * exp g i * o)
    output_tables = Bytes (2 * m * o)

def LutAToBType =
  block
    StartTag "mAB "
    number_of_input_channels  = UInt8
    number_of_output_channels = UInt8
    Match [0,0]
    offset_first_B_curve      = BE32
    offset_to_matrix          = BE32
    offset_to_first_M_curve   = BE32
    offset_to_CLUT            = BE32
    offset_to_first_A_curve   = BE32
    data                      = GetStream

-- XXX: Why is this the same as the AB case?
def LutBToAType =
  block
    StartTag "mBA "
    number_of_input_channels  = UInt8
    number_of_output_channels = UInt8
    Match [0,0]
    offset_first_B_curve      = BE32
    offset_to_matrix          = BE32
    offset_to_first_M_curve   = BE32
    offset_to_CLUT            = BE32
    offset_to_first_A_curve   = BE32
    data                      = GetStream


def MultiProcessElementsType =
  block
    let s = GetStream   -- offsets are relative to here
    StartTag "mpet"
    number_of_input_channels      = BE16
    number_of_output_channels     = BE16
    number_of_processing_elements = BE32
    n = number_of_processing_elements as uint 64
    Guard (n > 0)
    let els = Many n PositionNumber
    elements = map (e in els)
                   (ChunkRelativeTo s (e.offset as uint 64) (e.size as uint 64))


-- XXX: Shall we reqiure that there are no left over bytes after the XYZ number?
def XYZType =
  block
    StartTag "XYZ "
    Many XYZNumber


-- XXX: the values can be parsed a bit more.
def MeasurementType =
  block
    StartTag "meas"
    standard_observer = BE32
    nCIEXYZ           = XYZNumber
    geometry          = BE32
    flare             = BE32
    illuminant        = BE32


def NamedColor2Type =
  block
    StartTag "ncl2"
    vendor_specific      = BE32
    let count            = BE32 as uint 64
    let number_of_coords = BE32 as uint 64
    prefix               = Chunk 32 (Only ASCII7)
    suffix               = Chunk 32 (Only ASCII7)
    names                = Many count (ColorName number_of_coords)

def ColorName m =
  block
    name_root     = Chunk 32 ASCII7
    pcs_coords    = Many 3 BE16
    device_coords = Many m BE16


-- This type seems to be broken, so we just don't parse it.
-- See: http://www.color.org/PSD_TechNote.pdf
def ProfileSequenceDescType = {
  StartTag "pseq";
}


def ViewConditionsType =
  block
    StartTag "view"
    illuminantXYZ = XYZNumber
    surroundXYZ   = XYZNumber
    illuminant    = BE32


def TagStructType =
  block
    let s = GetStream
    StartTag "tstr"
    struct_type_id = Many 4 UInt8
    let n = BE32 as uint 64
    Many n
      block
        let ent = TagEntry
        LookAhead
          block
            SetStream s
            ParseTag ent

--------------------------------------------------------------------------------
-- Stuff that should be in a library somewhere

-- Get a chunk relative to the given stream.  Modifies the stream.
def ChunkRelativeTo s off sz =
  block
    SetStreamAt off s
    Bytes sz

def exp b e = for (x = 1; i in rangeUp(e)) x * b
