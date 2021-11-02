-- Referece: https://www.color.org/specification/ICC.2-2019.pdf
-- Reference for older features: https://www.color.org/icc32.pdf

import Daedalus
-- import Debug

def Main =
  block
    let s  = GetStream
    header = ProfileHeader
    tags   = TagTable s


--------------------------------------------------------------------------------
-- Header (Section 7.2)

def ProfileHeader =
  block
    size                = BEUInt32
    preferred_cmm_type  = Many 4 UInt8
    version             = VersionField
    devce_class         = ProfileClass
    color_space         = DataColorSpace
    pcs                 = DataColorSpace  -- check additional constraints?
    creation_date_time  = DateTimeNumber
    Match "acsp"
    primary_platform    = PrimaryPlatform
    profile_flags       = BEUInt32            -- XXX: bitdata, see 7.2.13
    device_manufacturer = Many 4 UInt8
    device_model        = Many 4 UInt8
    device_attributes   = BEUInt64           -- XXX: bitdata, see 7.2.16
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
  case BEUInt32 of
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
    let tag = BEUInt32
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
  case BEUInt32 of
    0s"APPL" -> {| apple_computer_inc    |}
    0s"MSFT" -> {| microsoft_corporation |}
    0s"SGI " -> {| silicon_graphics_inc  |}
    0s"SUNW" -> {| sun_microsystems      |}
    0        -> {| none                  |}


def RenderingIntent =
  case BEUInt32 of
    0 -> {| perceptual |}
    1 -> {| media_relative_colorimetric |}
    2 -> {| saturation |}
    3 -> {| icc_absolute_colorimetric |}


--------------------------------------------------------------------------------
-- Tag table (Section 7.3)

def TagTable s =
  Many (BEUInt32 as ?auto)
    block
      let sig = BEUInt32
      Positioned s (Tag sig)


--------------------------------------------------------------------------------
-- Tag Definitions (Section 9)

def Tag (sig : uint 32) =
  case sig of
    0s"desc" -> {| desc = LaxTextType |}

    0s"A2B0" -> {| A2B0 = LutAB_or_multi |}
    0s"A2B1" -> {| A2B1 = LutAB_or_multi |}
    0s"A2B2" -> {| A2B2 = LutAB_or_multi |}
    0s"A2B3" -> {| A2B3 = LutAB_or_multi |}
    0s"A2M0" -> {| A2M0 = MultiProcessElementsType |}

    0s"B2A0" -> {| B2A0 = LutBA_or_multi |}
    0s"B2A1" -> {| B2A1 = LutBA_or_multi |}
    0s"B2A2" -> {| B2A2 = LutBA_or_multi |}
    0s"B2A3" -> {| B2A3 = LutBA_or_multi |}

    0s"B2D0" -> {| B2D0 = MultiProcessElementsType |}
    0s"B2D1" -> {| B2D1 = MultiProcessElementsType |}
    0s"B2D2" -> {| B2D2 = MultiProcessElementsType |}
    0s"B2D3" -> {| B2D3 = MultiProcessElementsType |}

    0s"wtpt" -> {| wtpt = XYZType |}
    0s"cprt" -> {| cprt = LaxTextType |}
    0s"c2sp" -> {| c2sp = MultiProcessElementsType |}
    0s"s2cp" -> {| s2cp = MultiProcessElementsType |}

    0s"svcn" -> {| svcn = Only SpectralViewingConditionsType |}

    0s"rXYZ" -> {| rXYZ = XYZType |}
    0s"gXYZ" -> {| gXYZ = XYZType |}
    0s"bXYZ" -> {| bXYZ = XYZType |}

    0s"rTRC" -> {| rTRC = CurveOrPCurve |}
    0s"gTRC" -> {| gTRC = CurveOrPCurve |}
    0s"bTRC" -> {| bTRC = CurveOrPCurve |}

    0s"dmdd" -> {| dmdd = LaxTextType |}
    0s"dmnd" -> {| dmnd = LaxTextType |}

    0s"chrm" -> {| chrm = Only ChromaticityType |}
    0s"chad" -> {| chad = Only S15Fixed16ArrayType |}

    -- XXX: more ttags
    _        -> {| unimplemented = explode32 sig |}
  <| {| invalid_tag = InvalidTag sig |}

def InvalidTag sig =
  block
    sig = explode32 sig
    data = Many UInt8


--------------------------------------------------------------------------------
-- Various kinds of "numbers"  (Section 4)

def XYZNumber =
  block
    x = BESInt32
    y = BESInt32
    z = BESInt32

def XYNumber =
  block
    x = BEUInt32
    y = BEUInt32

def DateTimeNumber =
  block
    year    = BEUInt16
    month   = BEUInt16
    day     = BEUInt16
    hour    = BEUInt16
    minute  = BEUInt16
    second  = BEUInt16

def PositionNumber =
  block
    offset = BEUInt32 as uint 64
    size   = BEUInt32 as uint 64

-- 0 terminated ASCII 7 string (sem value does not include the 0)
def ASCII7 =
  block
    $$ = Many ($[1..] as? uint 7)
    Many (1 ..) $[0] <| Fail "Non 0 string terminator"

def Response16Number =
  block
    device = BEUInt16
    Exactly 0 BEUInt16
    measurement = BEUInt32


--------------------------------------------------------------------------------
-- Shared Tag Bodies

def LutAB_or_multi =
  Only
  First
    lutAB = LutAToBType
    mpe   = MultiProcessElementsType

def LutBA_or_multi =
  Only
  First
    lutBA = LutAToBType
    mpe   = MultiProcessElementsType

def CurveOrPCurve =
  First
    curve  = CurveType
    pcurve = ParametricCurveType

{- Various text encodings.
We combine them here because various versions of the spec do text
differently.  Depending on what one is doing this could be done differntly,
for example, have a separate `Tag` function for each version of the standard. -}
def LaxTextType =
  First
    uni  = MultiLocalizedUnicodeType
    desc = Only TextDescriptionType
    text = Only TextType

def MultiProcessElementsType = Only MPElement




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
    let record_number = BEUInt32
    Exactly 12 BEUInt32
    Many (record_number as ?auto) (UnicodeRecord s)

def UnicodeRecord s =
  block
    language    = Many 2 UInt8
    country     = Many 2 UInt8
    let size    = BEUInt32 as ?auto
    let offset  = BEUInt32 as ?auto
    data        = LookAhead
                    block
                      SetStreamAt offset s
                      Many size UInt8

def S15Fixed16ArrayType =
  block
    StartTag "sf32"
    Many BEUInt32     -- fixed point rationals

def ChromaticityType =
  block
    StartTag "chrm"
    let number_of_device_channels = BEUInt16 as ?auto
    phosphor_or_colorant          = BEUInt16
    cie_coords                    = Many number_of_device_channels XYNumber

def ColorantOrderType =
  block
    StartTag "clro"
    let count_of_colorants = BEUInt32
    Many UInt8

def ColorantTableType =
  block
    StartTag "clrt"
    let count_of_colorant = BEUInt32 as ?auto
    Many count_of_colorant Colorant

def Colorant =
  block
    name = Chunk 32 (Only ASCII7)
    pcs  = Many 3 BEUInt16

def CurveType =
  block
    StartTag "curv"
    let n = BEUInt32 as ?auto
    Many n BEUInt16

def ParametricCurveType =
  block
    StartTag "para"
    function = BEUInt16
    Match [0,0]
    parameters = Many BEUInt32
    -- These are to be interpreted as fixed precision rationals

def ResponseCurveSet16Type =
  block
    let s = GetStream
    StartTag "rcs2"
    let number_of_channels = BEUInt16 as uint 64
    let count              = BEUInt16 as uint 64
    Many count
      block
        let off = BEUInt32 as uint 64
        LookAhead
          block
            SetStreamAt off s
            ResponseCurve (number_of_channels)

def ResponseCurve n =
  block
    measurement_unit  = BEUInt32
    let counts        = Many n BEUInt32
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
    encoded_e_parameters = Many 9 (BEUInt32 as! sint 32)
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
    encoded_e_parameters = Many 9 (BEUInt32 as! sint 32)
    number_of_input_table_entries = BEUInt32
    let n = number_of_input_table_entries as uint 64
    number_of_output_table_entries = BEUInt32
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
    offset_first_B_curve      = BEUInt32
    offset_to_matrix          = BEUInt32
    offset_to_first_M_curve   = BEUInt32
    offset_to_CLUT            = BEUInt32
    offset_to_first_A_curve   = BEUInt32
    data                      = GetStream

-- XXX: Why is this the same as the AB case?
def LutBToAType =
  block
    StartTag "mBA "
    number_of_input_channels  = UInt8
    number_of_output_channels = UInt8
    Match [0,0]
    offset_first_B_curve      = BEUInt32
    offset_to_matrix          = BEUInt32
    offset_to_first_M_curve   = BEUInt32
    offset_to_CLUT            = BEUInt32
    offset_to_first_A_curve   = BEUInt32
    data                      = GetStream



def SpectralViewingConditionsType =
  block
    StartTag "svcn"
    colometric_observer = BEUInt32

def XYZType =
  Only
  block
    StartTag "XYZ "
    Many block
           $$ = XYZNumber
           Guard ($$.x >= 0) <| Fail "x value needs to be positive"
           Guard ($$.y >= 0) <| Fail "y value needs to be positive"
           Guard ($$.z >= 0) <| Fail "z value needs to be positive"


-- XXX: the values can be parsed a bit more.
def MeasurementType =
  block
    StartTag "meas"
    standard_observer = BEUInt32
    nCIEXYZ           = XYZNumber
    geometry          = BEUInt32
    flare             = BEUInt32
    illuminant        = BEUInt32


def NamedColor2Type =
  block
    StartTag "ncl2"
    vendor_specific      = BEUInt32
    let count            = BEUInt32 as uint 64
    let number_of_coords = BEUInt32 as uint 64
    prefix               = Chunk 32 (Only ASCII7)
    suffix               = Chunk 32 (Only ASCII7)
    names                = Many count (ColorName number_of_coords)

def ColorName m =
  block
    name_root     = Chunk 32 ASCII7
    pcs_coords    = Many 3 BEUInt16
    device_coords = Many m BEUInt16



def ViewConditionsType =
  block
    StartTag "view"
    illuminantXYZ = XYZNumber
    surroundXYZ   = XYZNumber
    illuminant    = BEUInt32

--------------------------------------------------------------------------------
-- Text Description Type (Section 6.5.13 of the 3.2 spec)

def TextDescriptionType =
  block
    StartTag "desc"
    ascii_data   = Many (BEUInt32 as ?auto) UInt8
    unicode_code = Many 4 UInt8
    unicode_data = Many (2 * BEUInt32 as ?auto) UInt8
    script_code  = Many 2 UInt8
    let script_len = BEUInt32 as ?auto
    script_data  = Many (BEUInt32 as ?auto) UInt8
    Many $[0]



--------------------------------------------------------------------------------
-- Multi Processing Elements (Section 11)


def MPElement =
  block
    head = MPElementHead
    body = MPElementBody head

def MPElementHead =
  block
    offset = GetStream
    tag    = BEUInt32
    Match [0,0,0,0]
    inputs  = BEUInt16 as uint 64
    outputs = BEUInt16 as uint 64


def MPElementBody head =
  case head.tag of
    0s"calc" -> {| calc = block
                            $$ = CalcElement head
                            -- LookAhead (CheckFunOpsInCalc $$)
                |}
    0s"cvst" -> {| cvst = Many head.inputs (Positioned head.offset Curve) |}
    0s"matf" -> {| matf = Matrix head.inputs head.outputs |}
    0s"mpet" -> {| mpet = block
                            let n = BEUInt32 as ?auto
                            Guard (n >= 1) <| Fail "Need at least one MPE"
                            Many n (Positioned head.offset MPElement)
                |}
    _        -> {| unimplemented = explode32 head.tag |}


--------------------------------------------------------------------------------
-- Calculator Elements (Section 11.2.1)


-- Table 85
def CalcElement head =
  block
    let subElNum  = BEUInt32 as ?auto
    inputs        = head.inputs   -- XXX: copied here to work around
    outputs       = head.outputs  -- a TC issue
    main          = Positioned head.offset CalcFun
    subElements   = Many subElNum (Positioned head.offset MPElement)


-- Table 86
def CalcFun =
  block
    StartTag "func"
    ManyFunOps (BEUInt32 as uint 64)


--------------------------------------------------------------------------------
-- Stack validation for FunOps


-- Initiial state of the stack
def funOpChecker = { stack = 0 : uint 64 }

-- Push arguments on the stack
def funOpPush n (x : funOpChecker) : funOpChecker =
  { stack = x.stack + n }

-- Remove some arguments from the stack
def FunOpPop op n (x : funOpChecker) : funOpChecker =
  block
    FunOpAssert op (x.stack >= n) "Not enough arguments for operation"
    { stack = x.stack - n }

-- Get the size of the stack
def funOpStackSize (x : funOpChecker) = x.stack

-- Check a certain condition and report an error if the condition fails
def FunOpAssert op b msg =
  First
    Guard b
    block
      SetStream op.offset
      Fail msg

-- Specifies effect of an instruction on the stack.
-- input arguments are removed; output arguments are pushed
def FunOpArgs op inArgs outArgs calc =
  funOpPush outArgs (FunOpPop op inArgs calc)
--------------------------------------------------------------------------------


def CheckFunOpsInCalc (c : CalcElement) =
  Guard (CheckFunOps c c.main funOpChecker == funOpChecker)
    <| Fail "Left over elements on the stack"

def CheckFunOps (c : CalcElement) (ops  : [FunOpWithPosition]) startCalc =
  for (calc = startCalc; op in ops)
    block
    SetStream op.offset
    case op.op of
      data    -> funOpPush 1 calc

      opIn p  ->
        block
          let n = p.t + 1
          FunOpAssert op (p.s < c.inputs && n <= (c.inputs - p.s) )
                                                     "Invalid channel in `in`"
          FunOpArgs op 0 n calc

      opOut p ->
        block
          let n = p.t + 1
          FunOpAssert op (p.s < c.outputs && n <= (c.outputs - p.s))
                                                    "Invalid channel in `out`"

          FunOpArgs op n 0 calc

      opTGet p -> FunOpArgs op 0 (p.t + 1) calc
      opTPut p -> FunOpArgs op (p.t + 1) 0 calc

      opTSave p ->
        block
          let n = p.t + 1
          FunOpArgs op n n calc

      opEnv -> FunOpArgs op 0 2 calc

      curv n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`curv` sub element index out of bounds"
          mpe.body is cvst <| Fail "`curv` argument is not a curve"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      mtx n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`mtx` sub element index out of bounds"
          mpe.body is matf <| Fail "`mtx` sub element is not a matrix"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      clut n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`clut` sub element index out of bounds"
          -- mpe.body is matf   XXX: CHECK TYPE
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      calc n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`calc` sub element index out of bounds"
          mpe.body is calc <| Fail "`calc` arguemtn is not a calculator"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      tint n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`tint` sub element index out of bounds"
          -- mpe.body is matf   XXX: CHECK TYPE
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      elem n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`elem` sub element index out of bounds"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      copy p -> FunOpArgs op (p.s + 1) ((p.s + 1) * (p.t + 2)) calc
      rotl p -> FunOpArgs op (p.s + 1) (p.s + 1) calc
      rotr p -> FunOpArgs op (p.s + 1) (p.s + 1) calc
      posd p -> FunOpArgs op (p.s + 1) ((p.s + 1) + (p.t + 1)) calc

      -- BUG in spec
      flip s -> FunOpArgs op (s + 2) (s + 2) calc

      pop s  -> FunOpArgs op (s+1) 0 calc
      solv p -> FunOpArgs op ((p.s + 1) * (p.t + 2)) (p.t + 2) calc

      tran p ->
        block
          let els = (p.t + 1) * (p.s + 1)
          FunOpArgs op els els calc

      sum n  -> FunOpArgs op (n+2) 1 calc
      prod n -> FunOpArgs op (n+2) 1 calc
      min n  -> FunOpArgs op (n+2) 1 calc
      max n  -> FunOpArgs op (n+2) 1 calc
      and n  -> FunOpArgs op (n+2) 1 calc
      or n   -> FunOpArgs op (n+2) 1 calc

      opPi      -> FunOpArgs op 0 1 calc
      opPosInf  -> FunOpArgs op 0 1 calc
      opNegInf  -> FunOpArgs op 0 1 calc
      opNaN     -> FunOpArgs op 0 1 calc

      opAdd s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opSub s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opMul s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opDiv s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opMod s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opPow s -> FunOpArgs op (2 * (s+1)) (s+1) calc

      opGamma s -> FunOpArgs op (s + 2) (s + 1) calc
      opSAdd s  -> FunOpArgs op (s + 2) (s + 1) calc
      opSSub s  -> FunOpArgs op (s + 2) (s + 1) calc
      opSMul s  -> FunOpArgs op (s + 2) (s + 1) calc
      opSDiv s  -> FunOpArgs op (s + 2) (s + 1) calc

      opSq   s  -> FunOpArgs op (s + 1) (s + 1) calc
      opSqrt s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCb   s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCbrt s  -> FunOpArgs op (s + 1) (s + 1) calc
      opAbs  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opNeg  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opRond s  -> FunOpArgs op (s + 1) (s + 1) calc
      opFlor s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCeil s  -> FunOpArgs op (s + 1) (s + 1) calc
      opTrnc s  -> FunOpArgs op (s + 1) (s + 1) calc
      opSign s  -> FunOpArgs op (s + 1) (s + 1) calc
      opExp  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opLog  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opLn   s  -> FunOpArgs op (s + 1) (s + 1) calc
      opSin  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCos  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opTan  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opASin s  -> FunOpArgs op (s + 1) (s + 1) calc
      opACos s  -> FunOpArgs op (s + 1) (s + 1) calc
      opATan s  -> FunOpArgs op (s + 1) (s + 1) calc
      opATn2 s  -> FunOpArgs op (s + 1) (s + 1) calc

      opCTop s  -> FunOpArgs op (2 * (s + 1)) (2 * (s + 1)) calc
      opPToc s  -> FunOpArgs op (2 * (s + 1)) (2 * (s + 1)) calc
      opRNum s  -> FunOpArgs op (s + 1) (s + 1)             calc

      opLT s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opLE s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opEQ s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opNear s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opGE s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opGT s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVMin s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVMax s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVAnd s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVOr s   -> FunOpArgs op (2 * (s+1)) (s+1) calc

      opTLab s  -> FunOpArgs op (3 * (s+1)) (3 * (s+1)) calc
      opTXYZ s  -> FunOpArgs op (3 * (s+1)) (3 * (s+1)) calc

      opIfThen thenOps ->
        block
          let calc1 = FunOpPop op 1 calc
          let calc2 = CheckFunOps c thenOps calc1
          FunOpAssert op (calc1 == calc2)
                        "`if-then` does not preserve the stack size"
          calc1

      opIfThenElse ops ->
        block
          let calc1 = FunOpPop op 1 calc
          let calc2 = CheckFunOps c ops.thenOps calc1
          let calc3 = CheckFunOps c ops.elseOps calc1
          FunOpAssert op (calc2 == calc3)
                        "`if-then-else` branches affect stack differently"
          calc2

      opSel ops ->
        block
          let calc1 = FunOpPop op 1 calc
          let res = CheckFunOps c ops.case1 calc1
          map (alt in ops.cases)
            (FunOpAssert op (CheckFunOps c alt calc1 == res)
                "`cases` in `sel` affect stack differently")
          case ops.dflt of
            nothing -> FunOpAssert op (res == calc1)
                        "`cases` in `sel` with no default do not preserve stack"
            just x  -> FunOpAssert op (CheckFunOps c x calc1 == res)
                         "`dflt` in `case` has different effect from `case`s"
          res


def FunOpWithPosition =
  block
    offset = GetStream
    op     = FunOp

def FunOp =
  block
    let tag = BEUInt32
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
      0s"env " -> {| opEnv   = BEUInt32 |}

      -- Table 93,94
      0s"curv" -> {| curv = OpParam as uint 64 |}    -- or a 16 bit number?
      0s"mtx " -> {| mtx  = OpParam as uint 64 |}
      0s"clut" -> {| clut = OpParam as uint 64 |}
      0s"calc" -> {| calc = OpParam as uint 64 |}
      0s"tint" -> {| tint = OpParam as uint 64 |}
      0s"elem" -> {| elem = OpParam as uint 64 |}

      -- Table 95,96
      0s"copy" -> {| copy = OpParams |}
      0s"rotl" -> {| rotl = OpParams |}
      0s"rotr" -> {| rotr = OpParams |}
      0s"posd" -> {| posd = OpParams |}
      0s"flip" -> {| flip = OpParam  |}
      0s"pop " -> {| pop  = OpParam  |}

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
      0s"pi  " -> {| opPi     = NoParams |}
      0s"+INF" -> {| opPosInf = NoParams |}
      0s"-INF" -> {| opNegInf = NoParams |}
      0s"NaN " -> {| opNaN    = NoParams |}
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
      0s"near" -> {| opNear   = OpParam |}
      0s"ge  " -> {| opGE     = OpParam |}
      0s"gt  " -> {| opGT     = OpParam |}
      0s"vmin" -> {| opVMin   = OpParam |}
      0s"vmax" -> {| opVMax   = OpParam |}
      0s"vand" -> {| opVAnd   = OpParam |}
      0s"vor " -> {| opVOr    = OpParam |}
      0s"tLab" -> {| opTLab   = OpParam |}
      0s"tXYZ" -> {| opTXYZ   = OpParam |}

      -- Table 103,104
      0s"if  " ->
        block
          let thenOps = BEUInt32 as uint 64
          case Optional (Match "else") of
            nothing -> {| opIfThen = ManyFunOps thenOps |}
            just    -> {| opIfThenElse = IfThenElse thenOps (BEUInt32 as uint 64) |}

      0s"else" -> Fail "`else` with no `if"

      -- Table 105
      0s"sel " ->
        block
          NoParams
          let c1 = SelCase
          let cs = Many SelCase
          let d  = Optional   block Match "dflt"; BEUInt32 as uint 64
          {| opSel = Sel c1 cs d |}

      0s"case" -> Fail "`case` with no `sel`"
      0s"dflt" -> Fail "`dflt` with no `sel`"

      _ -> Fail (concat [ "invalid tag: ", explode32 tag ])

def SelCase =
  block
    Match "case"
    BEUInt32 as uint 64

{- This extracts `n` 64-bit records.  It is used to parse a sequnce
   of `FunOp` becaues the `FunOp` parser consumes multiple "primitve"
   records at once. -}
def ManyFunOps n = Chunk (8 * n) (Only (Many FunOpWithPosition))

def IfThenElse thenOps elseOps =
  block
    thenOps = ManyFunOps thenOps
    elseOps = ManyFunOps elseOps

def Sel alt alts mbDflt =
  block
    case1 = ManyFunOps alt
    cases = map (n in alts) (ManyFunOps n)
    dflt  = case mbDflt of
              nothing -> nothing
              just n  -> just (ManyFunOps n)

def NoParams = @ block Exactly 0 BEUInt32

def OpParam =
  block
    $$ = BEUInt16 as uint 64
    Exactly 0 BEUInt16

def OpParams =
  block
    s = BEUInt16 as uint 64
    t = BEUInt16 as uint 64



--------------------------------------------------------------------------------
-- Curve Set (Section 11.2.2)

def Curve =
  block
    let tag = BEUInt32
    Exactly 0 BEUInt32
    case tag of
      0s"sngf" -> {| sngf = SingleSampledCurve |}
      0s"curf" -> {| curf = SegmentedCurve |}
      _        -> {| unimplemented = explode32 tag |}


-- XXX: Table 108
def SingleSampledCurve =
  block
    n = BEUInt32
    f = BEUInt32
    l = BEUInt32
    e = BEUInt16
    ty = BEUInt16


def SegmentedCurve =
  block
    let n = BEUInt16 as uint 64
    Guard (n >= 1) <| Fail "Need at least one curve segment"
    Exactly 0 BEUInt16
    let bnum = n - 1
    breakPoints = Many bnum BEFloat
    segments    = Many n CurveSegment

def CurveSegment =
  block
    let tag = BEUInt32
    Exactly 0 BEUInt32
    case tag of
      0s"parf" -> {| parf = FormualCurveSegment |}
      0s"samf" -> {| samf = Many (BEUInt32 as uint 64) BEFloat |} -- Table 112


-- | Table 110,111
def FormualCurveSegment =
  block
    let fun = BEUInt16
    Guard (BEUInt16 == 0)
    case fun of
      0 -> block fun = fun; args = Many 4 BEFloat
      1 -> block fun = fun; args = Many 5 BEFloat
      2 -> block fun = fun; args = Many 5 BEFloat
      3 -> block fun = fun; args = Many 4 BEFloat


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


