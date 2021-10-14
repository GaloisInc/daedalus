import Daedalus

-- ENTRY
def Main =
  block
    profileHeader = ProfileHeader
    tagTable      = TagTable


--------------------------------------------------------------------------------
-- Header (Section 7.2)

def ProfileHeader =
  block
    size                = BE32
    preferred_cmm_type  = BE32
    version             = VersionField
    devce_class         = ProfileClasses
    color_space         = DataColorSpaces
    pcs                 = DataColorSpaces  -- check additional constraints?
    creation_date_time  = DateTimeNumber
    Match "acsp"
    primary_platform    = PrimaryPlatforms
    profile_flags       = BE32
    device_manufacturer = BE32
    device_model        = BE32
    device_attributes   = BE64
    rendering_intent    = RenderingIntent
    illuminant          = XYZNumber
    creatior            = BE32
    identifier          = Many 16 UInt8
    reserved_data       = Many 28 $[0]

def VersionField =
  block
    major        = UInt8
    let min_bf   = UInt8
    minor        = min_bf >> 4 as! uint 4
    bugfix       = min_bf      as! uint 4
    Match [0x00, 0x00]

def ProfileClasses =
  First
    input_device_profile   = @Match "scnr"
    display_device_profile = @Match "mntr"
    output_device_profile  = @Match "prtr"
    device_link_profile    = @Match "link"
    color_space_profile    = @Match "spac"
    abstract_profile       = @Match "abst"
    named_color_profile    = @Match "nmcl"

def DataColorSpaces =
  First
    nciexyz_or_pcsxyz = @Match "XYZ "
    cielab_or_pcslab  = @Match "Lab "
    cieluv            = @Match "Luv "
    ycbcr             = @Match "Ycbr"
    cieyxy            = @Match "Yxy "
    rgb               = @Match "RGB "
    gray              = @Match "GRAY"
    hsv               = @Match "HSV "
    hls               = @Match "HLS "
    cmyk              = @Match "CMYK"
    cmy               = @Match "CMY "
    two_colour        = @Match "2CLR"
    three_colour      = @Match "3CLR"
    four_colour       = @Match "4CLR"
    five_colour       = @Match "5CLR"
    six_colour        = @Match "6CLR"
    seven_colour      = @Match "7CLR"
    eight_colour      = @Match "8CLR"
    nine_colour       = @Match "9CLR"
    ten_colour        = @Match "ACLR"
    eleven_colour     = @Match "BCLR"
    twelve_colour     = @Match "CCLR"
    thirteen_colour   = @Match "DCLR"
    fourteen_colour   = @Match "ECLR"
    fifteen_colour    = @Match "FCLR"


def PrimaryPlatforms =
  First
    none                  = @Match [0,0,0,0]
    apple_computer_inc    = @Match "APPL"
    microsoft_corporation = @Match "MSFT"
    silicon_graphics_inc  = @Match "SGI "
    sun_microsystems      = @Match "SUNW"


def RenderingIntent =
  First
    perceptual                  = @Match [0,0,0,0]
    media_relative_colorimetric = @Match [0,0,0,1]
    saturation                  = @Match [0,0,0,2]
    icc_absolute_colorimetric   = @Match [0,0,0,3]


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




--------------------------------------------------------------------------------
-- Tag table (Section 7.3)

def TagTable =
  block
    let n = BE32 as uint 64
    Many n TagEntry

def TagEntry =
  block
    tag_signature           = TagSignature
    offset_to_data_element  = BE32
    size_of_data_element    = BE32

def TagSignature =
  First
    A2B0 = @Match "A2B0"
    A2B1 = @Match "A2B1"
    A2B2 = @Match "A2B2"
    A2B3 = @Match "A2B3"
    A2M0 = @Match "A2M0"

    bcp0 = @Match "bcp0"
    bcp1 = @Match "bcp1"
    bcp2 = @Match "bcp2"
    bcp3 = @Match "bcp3"

    bsp0 = @Match "bsp0"
    bsp1 = @Match "bsp1"
    bsp2 = @Match "bsp2"
    bsp3 = @Match "bsp3"

    bXYZ = @Match "bXYZ"
    bTRC = @Match "bTRC"
    B2A0 = @Match "B2A0"
    B2A1 = @Match "B2A1"
    B2A2 = @Match "B2A2"
    B2D0 = @Match "B2D0"
    B2D1 = @Match "B2D1"
    B2D2 = @Match "B2D2"
    B2D3 = @Match "B2D3"
    calt = @Match "calt"
    targ = @Match "targ"
    chad = @Match "chad"
    clro = @Match "clro"
    clrt = @Match "clrt"
    clot = @Match "clot"
    ciis = @Match "ciis"
    cprt = @Match "cprt"
    dmnd = @Match "dmnd"
    dmdd = @Match "dmdd"
    D2B0 = @Match "D2B0"
    D2B1 = @Match "D2B1"
    D2B2 = @Match "D2B2"
    D2B3 = @Match "D2B3"
    gamt = @Match "gamt"
    kTRC = @Match "kTRC"
    gXYZ = @Match "gXYZ"
    gTRC = @Match "gTRC"
    lumi = @Match "lumi"
    meas = @Match "meas"
    wtpt = @Match "wtpt"
    ncl2 = @Match "ncl2"
    resp = @Match "resp"
    rig0 = @Match "rig0"
    pre0 = @Match "pre0"
    pre1 = @Match "pre1"
    pre2 = @Match "pre2"
    desc = @Match "desc"
    pseq = @Match "pseq"
    psid = @Match "psid"
    rXYZ = @Match "rXYZ"
    rTRC = @Match "rTRC"
    rig2 = @Match "rig2"
    tech = @Match "tech"
    vued = @Match "vued"
    view = @Match "view"




-- ENTRY: Assumes that the offsets are relative to the current stream.
def ParseTag (t : TagEntry) =
  block
    Skip (t.offset_to_data_element as uint 64);
    Chunk (t.size_of_data_element as uint 64) (Tag t.tag_signature);



--------------------------------------------------------------------------------
-- Tag Definitions (Section 9)

def Tag (sig : TagSignature) =
  case sig of
    A2B0 -> {| A2B0 = LutAB_or_multi |}
    A2B1 -> {| A2B1 = LutAB_or_multi |}
    A2B2 -> {| A2B2 = LutAB_or_multi |}
    A2B3 -> {| A2B3 = LutAB_or_multi |}
    A2M0 -> {| A2Mo = MultiProcessElementsType |}


    bXYZ -> {| bXYZ = XYZType |}
    bTRC -> {| bTRC = SomeCurve |}
    B2A0 -> {| B2A0 = Lut_8_16_BA |}
    B2A1 -> {| B2A1 = Lut_8_16_BA |}
    B2A2 -> {| B2A2 = Lut_8_16_BA |}
    B2D0 -> {| B2D0 = MultiProcessElementsType |}
    B2D1 -> {| B2D1 = MultiProcessElementsType |}
    B2D2 -> {| B2D2 = MultiProcessElementsType |}
    B2D3 -> {| B2D3 = MultiProcessElementsType |}
    calt -> {| calt = DateTimeType |}
    targ -> {| targ = TextType |}
    chad -> {| chad = S15Fixed16ArrayType |}
    clro -> {| clro = ColorantOrderType |}
    clrt -> {| clrt = ColorantTableType |}
    clot -> {| clot = ColorantTableType |}
    ciis -> {| ciis = SignatureType |}
    cprt -> {| cprt = MultiLocalizedUnicodeType |}
    dmnd -> {| dmnd = MultiLocalizedUnicodeType |}
    dmdd -> {| dmdd = MultiLocalizedUnicodeType |}
    D2B0 -> {| D2B0 = MultiProcessElementsType |}
    D2B1 -> {| D2B1 = MultiProcessElementsType |}
    D2B2 -> {| D2B2 = MultiProcessElementsType |}
    D2B3 -> {| D2B3 = MultiProcessElementsType |}
    gamt -> {| gamt = Lut_8_16_BA |}
    kTRC -> {| kTRC = SomeCurve |}
    gXYZ -> {| gXYZ = XYZType |}
    gTRC -> {| gTRC = SomeCurve |}
    lumi -> {| lumi = XYZType |}
    meas -> {| meas = MeasurementType |}
    wtpt -> {| wtpt = XYZType |}
    ncl2 -> {| ncl2 = NamedColor2Type |}
    resp -> {| resp = ResponseCurveSet16Type |}
    rig0 -> {| rig0 = SignatureType |}
    pre0 -> {| pre0 = Lut_8_16_AB_BA |}
    pre1 -> {| pre1 = Lut_8_16_BA |}
    pre2 -> {| pre2 = Lut_8_16_BA |}
    desc -> {| desc = MultiLocalizedUnicodeType |}
    pseq -> {| pseq = ProfileSequenceDescType |} -- XXX
    psid -> {| psid = {} |} -- XXX
    rXYZ -> {| rXYZ = XYZType |}
    rTRC -> {| rTRC = SomeCurve |}
    rig2 -> {| rig2 = SignatureType |}
    tech -> {| tech = SignatureType |}
    vued -> {| vued = MultiLocalizedUnicodeType |}
    view -> {| view = ViewConditionsType |}

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
