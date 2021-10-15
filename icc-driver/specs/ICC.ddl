import Daedalus

-- ENTRY
def Main =
  block
    let s = GetStream
    profileHeader = ProfileHeader
    tags = map (entry in TagTable) (ParseTagIn s entry)


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
    creator             = Many 4 UInt8
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

def TagTable = Many (BE32 as uint 64) TagEntry

def TagEntry =
  block
    tag_signature           = BE32
    offset_to_data_element  = BE32 as uint 64
    size_of_data_element    = BE32 as uint 64


def ParseTagIn s (t : TagEntry) =
  block
    SetStreamAt t.offset_to_data_element s
    Chunk t.size_of_data_element (Tag t.tag_signature);

-- ENTRY: Assumes that the offsets are relative to the current stream.
def ParseTag (t : TagEntry) =
  block
    Skip t.offset_to_data_element
    Chunk t.size_of_data_element (Tag t.tag_signature);



--------------------------------------------------------------------------------
-- Tag Definitions (Section 9)

def Tag (sig : uint 32) =
  case sig of
    0s"desc" -> {| desc = MultiLocalizedUnicodeType |}

    0s"A2B0" -> {| A2B0 = LutAB_or_multi |}
    0s"A2B1" -> {| A2B1 = LutAB_or_multi |}
    0s"A2B2" -> {| A2B2 = LutAB_or_multi |}
    0s"A2B3" -> {| A2B3 = LutAB_or_multi |}
    0s"A2M0" -> {| A2Mo = MultiProcessElementsType |}

    0s"B2A0" -> {| B2A0 = LutBA_or_multi |}
    0s"B2A1" -> {| B2A1 = LutBA_or_multi |}
    0s"B2A2" -> {| B2A2 = LutBA_or_multi |}
    0s"B2A3" -> {| B2A3 = LutBA_or_multi |}

    0s"B2D0" -> {| B2D0 = MultiProcessElementsType |}
    0s"B2D1" -> {| B2D1 = MultiProcessElementsType |}
    0s"B2D2" -> {| B2D2 = MultiProcessElementsType |}
    0s"B2D3" -> {| B2D3 = MultiProcessElementsType |}

    0s"wtpt" -> {| wtpt = XYZType |}
    0s"cprt" -> {| cprt = MultiLocalizedUnicodeType |}
    0s"c2sp" -> {| c2sp = MultiProcessElementsType |}
    0s"s2cp" -> {| s2sp = MultiProcessElementsType |}

    0s"svcn" -> {| svcn = SpectralViewingConditionsType |}

{-

    0s"bXYZ" -> {| bXYZ = XYZType |}
    0s"bTRC" -> {| bTRC = SomeCurve |}
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
    -- 0s"ncl2" -> {| ncl2 = NamedColor2Type |}
    0s"resp" -> {| resp = ResponseCurveSet16Type |}
    0s"rig0" -> {| rig0 = SignatureType |}
    0s"pre0" -> {| pre0 = Lut_8_16_AB_BA |}
    0s"pre1" -> {| pre1 = Lut_8_16_BA |}
    0s"pre2" -> {| pre2 = Lut_8_16_BA |}

    0s"psid" -> {| psid = {} |} -- XXX
    0s"rXYZ" -> {| rXYZ = XYZType |}
    0s"rTRC" -> {| rTRC = SomeCurve |}
    0s"rig2" -> {| rig2 = SignatureType |}
    0s"tech" -> {| tech = SignatureType |}
    0s"vued" -> {| vued = MultiLocalizedUnicodeType |}
    0s"view" -> {| view = ViewConditionsType |}
-}
    _        -> {| unknown = explode32 sig |}




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
    offset = BE32 as uint 64
    size   = BE32 as uint 64

-- 0 terminated ASCII 7 string (sem value does not include the 0)
def ASCII7 =
  block
    $$ = Many ($[1..] as? uint 7)
    Many (1 ..) $[0] <| Fail "Non 0 string terminator"

def Response16Number =
  block
    device = BE16
    Exactly 0 BE16
    measurement = BE32

-- def SpectralRange =
--   block
    


--------------------------------------------------------------------------------

def LutAB_or_multi =
  First
    lutAB = LutAToBType
    mpe   = MultiProcessElementsType

def LutBA_or_multi =
  First
    lutBA = LutAToBType
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
    Exactly 12 BE32
    Many (record_number as uint 64) (UnicodeRecord s)

def UnicodeRecord s =
  block
    language    = Many 2 UInt8
    country     = Many 2 UInt8
    let size    = BE32 as uint 64
    let offset  = BE32 as uint 64
    data        = LookAhead
                    block
                      SetStreamAt offset s
                      Many size UInt8

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
    Many UInt8

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
    let n = number_of_processing_elements as uint 64
    Guard (n >= 1) <| Fail "Need at least one MPE"
    elements = Many n (Positioned s MPElement)



def SpectralViewingConditionsType =
  block
    StartTag "svcn"
    colometric_observer = BE32

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
-- Multi Processing Elements (Section 11)


def MPElement =
  block
    let s = GetStream
    let tag = BE32
    Match [0,0,0,0]
    let inputs  = BE16 as uint 64
    let outputs = BE16 as uint 64
    case tag of
      0s"calc" -> {| calc = CalcElement s inputs outputs |}
      0s"cvst" -> {| cvst = Many inputs (Positioned s Curve) |}
      0s"matf" -> {| matf = Matrix inputs outputs |}
      _ -> {| unknown = explode32 tag |}


--------------------------------------------------------------------------------
-- Calculator Elements (Section 11.2.1)

-- Table 85
def CalcElement s inputs outputs =
  block
    let subElNum  = BE32 as uint 64
    inputs        = inputs
    outputs       = outputs
    main          = Positioned s CalcFun
    subElements   = Many subElNum (Positioned s MPElement)


-- Table 86
def CalcFun =
  block
    StartTag "func"
    Many (BE32 as uint 64) FunOp

def FunOp =
  block
    let tag = BE32
    case tag of

      -- Table 87
      0s"data" -> {| data    = BEFloat |}

      -- Table 88,89
      0s"in  " -> {| opIn    = OpParams |}
      0s"out " -> {| opOut   = OpParams |}
      0s"tget" -> {| opTGet  = OpParams |}
      0s"tput" -> {| opTPut  = OpParams |}
      0s"tsav" -> {| opTSave = OpParams |}

      -- Table 90,91
      0s"env " -> {| opEnv   = BE32 |}

      -- Table 94
      0s"curv" -> {| curv = BE32 |}
      0s"mtx " -> {| mtx  = BE32 |}
      0s"clut" -> {| clut = BE32 |}
      0s"calc" -> {| calc = BE32 |}
      0s"tint" -> {| tint = BE32 |}
      0s"elem" -> {| elem = BE32 |}

      -- Table 95,96
      0s"copy" -> {| copy = OpParams |}
      0s"rotl" -> {| rotl = OpParams |}
      0s"rotr" -> {| rotr = OpParams |}
      0s"posd" -> {| posd = OpParams |}
      0s"flip" -> {| flip = OpParams |}
      0s"pop " -> {| pop  = OpParams |}

      -- Table 97,98
      0s"solv" -> {| solv = OpParams |}
      0s"tran" -> {| tran = OpParams |}

      -- Table 99,100
      0s"sum " -> {| sum  = OpParam |}
      0s"prod" -> {| prod = OpParam |}
      0s"min " -> {| min  = OpParam |}
      0s"max " -> {| max  = OpParam |}
      0s"and " -> {| and  = OpParam |}
      0s"or  " -> {| or   = OpParam |}

      -- Table 101,102
      0s"pi  " -> {| opPi     = OpParam |}
      0s"+INF" -> {| opPosInf = OpParam |}
      0s"-INF" -> {| opNegInf = OpParam |}
      0s"NAN " -> {| opNAN    = OpParam |}
      0s"add " -> {| opAdd    = OpParam |}
      0s"sub " -> {| opSub    = OpParam |}
      0s"mul " -> {| opMul    = OpParam |}
      0s"div " -> {| opDiv    = OpParam |}
      0s"mod " -> {| opMod    = OpParam |}
      0s"pow " -> {| opPow    = OpParam |}
      0s"gama" -> {| opGamma  = OpParam |}
      0s"sadd" -> {| opSAdd   = OpParam |}
      0s"ssub" -> {| opSSub   = OpParam |}
      0s"smul" -> {| opSMul   = OpParam |}
      0s"sdiv" -> {| opSDiv   = OpParam |}
      0s"sq  " -> {| opSq     = OpParam |}
      0s"sqrt" -> {| opSqrt   = OpParam |}
      0s"cb  " -> {| opCb     = OpParam |}
      0s"cbrt" -> {| opCbrt   = OpParam |}
      0s"abs " -> {| opAbs    = OpParam |}
      0s"neg " -> {| opNeg    = OpParam |}
      0s"rond" -> {| opRond   = OpParam |}
      0s"flor" -> {| opFlor   = OpParam |}
      0s"ceil" -> {| opCeil   = OpParam |}
      0s"trnc" -> {| opTrnc   = OpParam |}
      0s"sign" -> {| opSign   = OpParam |}
      0s"exp " -> {| opExp    = OpParam |}
      0s"log " -> {| opLog    = OpParam |}
      0s"ln  " -> {| opLn     = OpParam |}
      0s"sin " -> {| opSin    = OpParam |}
      0s"cos " -> {| opCos    = OpParam |}
      0s"tan " -> {| opTan    = OpParam |}
      0s"asin" -> {| opASin   = OpParam |}
      0s"acos" -> {| opACos   = OpParam |}
      0s"atan" -> {| opATan   = OpParam |}
      0s"atn2" -> {| opATn2   = OpParam |}
      0s"ctop" -> {| opCTop   = OpParam |}
      0s"ptoc" -> {| opPToc   = OpParam |}
      0s"rnum" -> {| opRNum   = OpParam |}
      0s"lt  " -> {| opLT     = OpParam |}
      0s"le  " -> {| opLE     = OpParam |}
      0s"eq  " -> {| opEQ     = OpParam |}
      0s"near" -> {| opNer    = OpParam |}
      0s"ge  " -> {| opGE     = OpParam |}
      0s"gt  " -> {| opGT     = OpParam |}
      0s"vmin" -> {| opVMin   = OpParam |}
      0s"vmax" -> {| opVMax   = OpParam |}
      0s"vand" -> {| opVAnd   = OpParam |}
      0s"vor " -> {| opVOr    = OpParam |}
      0s"tLab" -> {| opTLab   = OpParam |}
      0s"tXYZ" -> {| opTXYZ   = OpParam |}

      -- Table 103,104
      0s"if  " -> {| opIf     = BE32 as uint 64 |}
      0s"else" -> {| opElse   = BE32 as uint 64 |}

      -- Table 105
      0s"sel " -> {| opSel    = NoParams |}
      0s"case" -> {| opCase   = BE32 as uint 64 |}
      0s"dflt" -> {| opDflt   = BE32 as uint 64 |}

      _        -> {| unknown  = block
                                  tag   = explode32 tag
                                  param = BE32
                  |}

def NoParams = Exactly 0 BE32

def OpParam =
  block
    $$ = BE16
    Exactly 0 BE16

def OpParams =
  block
    s = BE16
    t = BE16



--------------------------------------------------------------------------------
-- Curve Set (Section 11.2.2)

def Curve =
  block
    let tag = BE32
    Exactly 0 BE32
    case tag of
      0s"sngf" -> {| sngf = SingleSampledCurve |}
      0s"curf" -> {| curf = SegmentedCurve |}
      _        -> {| unknown = explode32 tag |}


-- XXX: Table 108
def SingleSampledCurve =
  block
    n = BE32
    f = BE32
    l = BE32
    e = BE16
    ty = BE16


def SegmentedCurve =
  block
    let n = BE16 as uint 64
    Guard (n >= 1) <| Fail "Need at least one curve segment"
    Exactly 0 BE16
    let bnum = n - 1
    breakPoints = Many bnum BEFloat
    segements   = Many n CurveSegment

def CurveSegment =
  block
    let tag = BE32
    Exactly 0 BE32
    case tag of
      0s"parf" -> {| parf = FormualCurveSegment |}
      0s"samf" -> {| samf = Many (BE32 as uint 64) BEFloat |} -- Table 112


-- | Table 110,111
def FormualCurveSegment =
  block
    let fun = BE16
    Guard (BE16 == 0)
    case fun of
      0 -> {| fun0 = FunParams_g_a_b_c   |}
      1 -> {| fun1 = FunParams_g_a_b_c_d |}
      2 -> {| fun2 = FunParams_a_b_c_d_e |}
      3 -> {| fun3 = FunParams_g_a_b_c   |}

def FunParams_g_a_b_c =
  block
    g = BEFloat
    a = BEFloat
    b = BEFloat
    c = BEFloat

def FunParams_g_a_b_c_d =
  block
    g = BEFloat
    a = BEFloat
    b = BEFloat
    c = BEFloat
    d = BEFloat

def FunParams_a_b_c_d_e =
  block
    a = BEFloat
    b = BEFloat
    c = BEFloat
    d = BEFloat
    e = BEFloat


--------------------------------------------------------------------------------
-- 11.2.10 Matrix Element

def Matrix p q =
  block
    matrix = Many q (Many p BEFloat)
    vector = Many q BEFloat


------------------------------------------------------------------------------
-- Misc. utils


def Exactly x P = Guard (P == x) <| Fail "Unexpected field value"


def explode32 (sig : uint 32) =
  [ sig >> 24 as! uint 8
  , sig >> 16 as! uint 8
  , sig >>  8 as! uint 8
  , sig       as! uint 8
  ]



-- Get a chunk relative to the given stream.  Modifies the stream.
def ChunkRelativeTo s off sz =
  block
    SetStreamAt off s
    Bytes sz

def exp b e = for (x = 1; i in rangeUp(e)) x * b

def Positioned s P =
  block
    let p = PositionNumber
    LookAhead
      block
        SetStreamAt p.offset s
        Chunk p.size P


