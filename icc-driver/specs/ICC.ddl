

-- ENTRY
def Main = {
  profileHeader = ProfileHeader;
  tagTable      = TagTable;
}



--------------------------------------------------------------------------------
-- Header (Section 7.2)

def ProfileHeader = {
  size                = BE32;
  preferred_cmm_type  = BE32;
  version             = VersionField;
  devce_class         = ProfileClasses;
  color_space         = DataColorSpaces;
  pcs                 = DataColorSpaces; -- check additional constraints?
  creation_date_time  = DateTimeNumber;
  Match "acsp";
  primary_platform    = PrimaryPlatforms;
  profile_flags       = BE32;
  device_manufacturer = BE32;
  device_model        = BE32;
  device_attributes   = BE64;
  rendering_intent    = RenderingIntent;
  illuminant          = XYZNumber;
  creatior            = BE32;
  identifier          = Many 16 UInt8;
  reserved_data       = Many 28 (Match1 0);
}

def VersionField = {
  major        = UInt8;
  @min_bf      = UInt8;
  minor        = ^ min_bf >> 4 as! uint 4;
  bugfix       = ^ min_bf as! uint 4;
  Match [0x00; 0x00];
}

def ProfileClasses = {
  Choose1 {
    input_device_profile   = @Match "scnr";
    display_device_profile = @Match "mntr";
    output_device_profile  = @Match "prtr";
    device_link_profile    = @Match "link";
    color_space_profile    = @Match "spac";
    abstract_profile       = @Match "abst";
    named_color_profile    = @Match "nmcl";
  }
}

def DataColorSpaces = {
  Choose1 {
    nciexyz_or_pcsxyz = @Match "XYZ ";
    cielab_or_pcslab  = @Match "Lab ";
    cieluv            = @Match "Luv ";
    ycbcr             = @Match "Ycbr";
    cieyxy            = @Match "Yxy ";
    rgb               = @Match "RGB ";
    gray              = @Match "GRAY";
    hsv               = @Match "HSV ";
    hls               = @Match "HLS ";
    cmyk              = @Match "CMYK";
    cmy               = @Match "CMY ";
    two_colour        = @Match "2CLR";
    three_colour      = @Match "3CLR";
    four_colour       = @Match "4CLR";
    five_colour       = @Match "5CLR";
    six_colour        = @Match "6CLR";
    seven_colour      = @Match "7CLR";
    eight_colour      = @Match "8CLR";
    nine_colour       = @Match "9CLR";
    ten_colour        = @Match "ACLR";
    eleven_colour     = @Match "BCLR";
    twelve_colour     = @Match "CCLR";
    thirteen_colour   = @Match "DCLR";
    fourteen_colour   = @Match "ECLR";
    fifteen_colour    = @Match "FCLR";
  }
}

def PrimaryPlatforms = {
  Choose1 {
  none                    = @Match [0,0,0,0];
    apple_computer_inc    = @Match "APPL";
    microsoft_corporation = @Match "MSFT";
    silicon_graphics_inc  = @Match "SGI ";
    sun_microsystems      = @Match "SUNW";
  }
}


def RenderingIntent = {
  Choose1 {
    perceptual                  = @Match [0,0,0,0];
    media_relative_colorimetric = @Match [0,0,0,1];
    saturation                  = @Match [0,0,0,2];
    icc_absolute_colorimetric   = @Match [0,0,0,3];
  }
}


--------------------------------------------------------------------------------
-- Various kinds of "numbers"  (Section 4)

def XYZNumber = {
  x = BE32;
  y = BE32;
  z = BE32;
}

def XYNumber = {
  x = BE32;
  y = BE32;
}

def DateTimeNumber = {
  year    = BE16;
  month   = BE16;
  day     = BE16;
  hour    = BE16;
  minute  = BE16;
  second  = BE16;
}

def PositionNumber = {
  offset = BE32;
  size   = BE32;
}

-- 0 terminated ASCII 7 string (sem value does not include the 0)
def ASCII7 = {
  $$ = Many (Match1 (1..) as? uint 7);
  Many (1 .. ) (Match1 0) <| Fail "Non 0 string terminator";
}

def Response16Number = {
  device = BE16;
  Match [0,0];
  measurement = BE32;
}





--------------------------------------------------------------------------------
-- Tag table (Section 7.3)

def TagTable = {
  @tag_count = BE32;
  Many (tag_count as uint 64) TagEntry;
}

def TagEntry = {
  tag_signature           = Many 4 UInt8;
  offset_to_data_element  = BE32;
  size_of_data_element    = BE32;
}

-- ENTRY: Should only be used when the stream is at offset 0
def ParseTag (t : TagEntry) = {
  Goto (t.offset_to_data_element as uint 64);
  ParseChunk (t.size_of_data_element as uint 64) (Tag t.tag_signature);
}



--------------------------------------------------------------------------------
-- Tag Definitions (Section 9)

def Tag sig = Choose1 {
  AToB0               = { Guard (sig == "A2B0"); commit; Lut_8_16_AB };
  AToB1               = { Guard (sig == "A2B1"); commit; Lut_8_16_AB };
  AToB2               = { Guard (sig == "A2B2"); commit; Lut_8_16_AB };
  blueMatrixColumn    = { Guard (sig == "bXYZ"); commit; XYZType };
  blueTRC             = { Guard (sig == "bTRC"); commit; SomeCurve };
  BToA0               = { Guard (sig == "B2A0"); commit; Lut_8_16_BA };
  BToA1               = { Guard (sig == "B2A1"); commit; Lut_8_16_BA };
  BToA2               = { Guard (sig == "B2A2"); commit; Lut_8_16_BA };
  BToD0               = { Guard (sig == "B2D0"); commit; MultiProcessElementsType };
  BToD1               = { Guard (sig == "B2D1"); commit; MultiProcessElementsType };
  BToD2               = { Guard (sig == "B2D2"); commit; MultiProcessElementsType };
  BToD3               = { Guard (sig == "B2D3"); commit; MultiProcessElementsType };
  calibrationDateTime = { Guard (sig == "calt"); commit; DateTimeType };
  charTarget          = { Guard (sig == "targ"); commit; TextType };
  chromaticAdaptation = { Guard (sig == "chad"); commit; S15Fixed16ArrayType };
  colorantOrder       = { Guard (sig == "clro"); commit; ColorantOrderType; };
  colorantTable       = { Guard (sig == "clrt"); commit; ColorantTableType; };
  colorantTableOut    = { Guard (sig == "clot"); commit; ColorantTableType; };
  colorimetricIntentImageState =
                        { Guard (sig == "ciis"); commit; SignatureType };
  copyright           = { Guard (sig == "cprt"); commit; MultiLocalizedUnicodeType };
  deviceMfgDesc       = { Guard (sig == "dmnd"); commit; MultiLocalizedUnicodeType };
  deviceModelDesc     = { Guard (sig == "dmdd"); commit; MultiLocalizedUnicodeType };
  DToB0               = { Guard (sig == "D2B0"); commit; MultiProcessElementsType };
  DToB1               = { Guard (sig == "D2B1"); commit; MultiProcessElementsType };
  DToB2               = { Guard (sig == "D2B2"); commit; MultiProcessElementsType };
  DToB3               = { Guard (sig == "D2B3"); commit; MultiProcessElementsType };
  gamut               = { Guard (sig == "gamt"); commit; Lut_8_16_BA };
  grayTRC             = { Guard (sig == "kTRC"); commit; SomeCurve };
  greenMatrixColumn   = { Guard (sig == "gXYZ"); commit; XYZType };
  greenTRC            = { Guard (sig == "gTRC"); commit; SomeCurve };
  luminance           = { Guard (sig == "lumi"); commit; XYZType };
  measurement         = { Guard (sig == "meas"); commit; MeasurementType };
  mediaWhitePoint     = { Guard (sig == "wtpt"); commit; XYZType };
  namedColor2         = { Guard (sig == "ncl2"); commit; NamedColor2Type };
  outputResponse      = { Guard (sig == "resp"); commit; ResponseCurveSet16Type };
  perceptualRenderingIntentGamut =
                        { Guard (sig == "rig0"); commit; SignatureType };
  preview0            = { Guard (sig == "pre0"); commit; Lut_8_16_AB_BA };
  preview1            = { Guard (sig == "pre1"); commit; Lut_8_16_BA };
  preview2            = { Guard (sig == "pre2"); commit; Lut_8_16_BA };
  profileDescription  = { Guard (sig == "desc"); commit; MultiLocalizedUnicodeType };
  profileSequenceDesc = { Guard (sig == "pseq"); commit; ProfileSequenceDescType }; -- XXX
  profileSequenceIdentifier =
                        { Guard (sig == "psid"); commit; {} }; -- XXX
  redMatrixColumn     = { Guard (sig == "rXYZ"); commit; XYZType; };
  redTRC              = { Guard (sig == "rTRC"); commit; SomeCurve };
  saturationRenderingIntentGamut =
                        { Guard (sig == "rig2"); commit; SignatureType };
  technology          = { Guard (sig == "tech"); commit; SignatureType };
  viewCondDesc        = { Guard (sig == "vued"); commit; MultiLocalizedUnicodeType };
  viewConditions      = { Guard (sig == "view"); commit; ViewConditionsType };
} <| Fail (concat [ "Unregonized tag: ", sig ])


def Lut_8_16_AB_BA = Choose1 {
  lut8  = Lut8Type;
  lut16 = Lut16Type;
  lutAB = LutAToBType;
  lutBA = LutBToAType;
}



def Lut_8_16_AB = Choose1 {
  lut8  = Lut8Type;
  lut16 = Lut16Type;
  lutAB = LutAToBType;
}

def Lut_8_16_BA = Choose1 {
  lut8  = Lut8Type;
  lut16 = Lut16Type;
  lutBA = LutBToAType;
}



def SomeCurve = Choose1 {
  curve = CurveType;
  parametric_curve = ParametricCurveType;
}



--------------------------------------------------------------------------------
-- Tag types (Section 10)

def StartTag x = { Match x; commit; Match [0,0,0,0] }

def DateTimeType = { StartTag "dtim"; DateTimeNumber; }

def TextType = { StartTag "text"; Only ASCII7; }

def SignatureType = { StartTag "sig "; Many 4 UInt8; }

def MultiLocalizedUnicodeType = {
  @s = GetStream;   -- Offsets are relative to here
  StartTag "mluc";
  @record_number = BE32;
  @record_size   = BE32;
  Guard (record_size == 12);
  Many (record_number as uint 64) (UnicodeRecord s);
}

def UnicodeRecord s = {
  language = BE16;
  country  = BE16;
  @size    = BE32;
  @offset  = BE32;
  data     = Remote (ChunkRelativeTo s (offset as uint 64) (size as uint 64));
}

def S15Fixed16ArrayType = {
  StartTag "sf32";
  Many BE32;    -- fixed point rationals
}

def ChromaticityType = {
  StartTag "chrm";
  @number_of_device_channels = BE16;
  phosphor_or_colorant       = BE16;
  cie_coords                 = Many (number_of_device_channels as uint 64) XYNumber;
}

def ColorantOrderType = {
  StartTag "clro";
  @count_of_colorants = BE32;
  Many UInt8;
}

def ColorantTableType = {
  StartTag "clrt";
  @count_of_colorant = BE32;
  Many (count_of_colorant as uint 64) Colorant;
}

def Colorant = {
  name = ParseChunk 32 (Only ASCII7);
  pcs  = Many 3 BE16;
}

def CurveType = {
  StartTag "curv";
  @n = BE32;
  Many (n as uint 64) BE16;
}

def ParametricCurveType = {
  StartTag "para";
  function = BE16;
  Match [0,0];
  parameters = Many BE32;
    -- These are to be interpreted as fixed precision rationals
}

def ResponseCurveSet16Type = {
  @s = GetStream;
  StartTag "rcs2";
  @number_of_channels = BE16;
  @count              = BE16;
  Many (count as uint 64) {
    @off  = BE32;
    Remote { GotoRel s (off as uint 64); ResponseCurve (number_of_channels as uint 64)};
  }
}

def ResponseCurve n = {
  measurement_unit  = BE32;
  @counts           = Many n BE32;
  pcxyzs            = Many n XYNumber;
  response_arrays   = map (qi in counts) (Many (qi as uint 64) Response16Number)
}

def Lut8Type = {
  StartTag "mft1";
  number_of_input_channels = UInt8;
  @i = ^ number_of_input_channels as uint 64;
  number_of_output_channels = UInt8;
  @o = ^ number_of_output_channels as uint 64;
  number_of_clut_grid_points = UInt8;
  @g = number_of_clut_grid_points as uint 64;
  Match1 0x00;
  encoded_e_parameters = Many 9 { @x = BE32; ^ x as! sint 32 };
  input_tables  = Chunk (256 * i);
  clut_values   = Chunk ((exp g i) * o);
  output_tables = Chunk (256 * o);
}

def Lut16Type = {
  StartTag "mft2";
  number_of_input_channels = UInt8;
  @i = ^ number_of_input_channels as uint 64;
  number_of_output_channels = UInt8;
  @o = ^ number_of_output_channels as uint 64;
  number_of_clut_grid_points = UInt8;
  @g = number_of_clut_grid_points as uint 64;
  Match1 0x00;
  encoded_e_parameters = Many 9 { @x = BE32; ^ x as! sint 32 };
  number_of_input_table_entries = BE32;
  @n = ^ number_of_input_table_entries as uint 64;
  number_of_output_table_entries = BE32;
  @m = ^ number_of_output_table_entries as uint 64;
  input_tables  = Chunk (256 * n * i);
  clut_values   = Chunk (2 * (exp g i) * o);
  output_tables = Chunk (2 * m * o);
}

def LutAToBType = {
  StartTag "mAB ";
  number_of_input_channels  = UInt8;
  number_of_output_channels = UInt8;
  Match [0,0];
  offset_first_B_curve      = BE32;
  offset_to_matrix          = BE32;
  offset_to_first_M_curve   = BE32;
  offset_to_CLUT            = BE32;
  offset_to_first_A_curve   = BE32;
  data                      = GetStream;
}

-- XXX: Why is this the same as the AB case?
def LutBToAType = {
  StartTag "mBA ";
  number_of_input_channels  = UInt8;
  number_of_output_channels = UInt8;
  Match [2,0];
  offset_first_B_curve      = BE32;
  offset_to_matrix          = BE32;
  offset_to_first_M_curve   = BE32;
  offset_to_CLUT            = BE32;
  offset_to_first_A_curve   = BE32;
  data                      = GetStream;
}


def MultiProcessElementsType = {
  @s = GetStream;   -- offsets are relative to here
  StartTag "mpet";
  number_of_input_channels      = BE16;
  number_of_output_channels     = BE16;
  number_of_processing_elements = BE32;
  n = ^ number_of_processing_elements as uint 64;
  Guard (n > 0);
  @els = Many n PositionNumber;
  elements = map (e in els)
                 (ChunkRelativeTo s (e.offset as uint 64) (e.size as uint 64));
}


-- XXX: Shall we reqiure that there are no left over bytes after the XYZ number?
def XYZType = {
  StartTag "XYZ ";
  Many XYZNumber;
}


-- XXX: the values can be parsed a bit more.
def MeasurementType = {
  StartTag "meas";
  standard_observer = BE32;
  nCIEXYZ           = XYZNumber;
  geometry          = BE32;
  flare             = BE32;
  illuminant        = BE32;
}


def NamedColor2Type = {
  StartTag "ncl2";
  vendor_specific   = BE32;
  @count            = BE32;
  @number_of_coords = BE32;
  prefix            = ParseChunk 32 (Only ASCII7);
  suffix            = ParseChunk 32 (Only ASCII7);
  names             = Many (count as uint 64) (ColorName (number_of_coords as uint 64));
}

def ColorName m = {
  name_root     = ParseChunk 32 ASCII7;
  pcs_coords    = Many 3 BE16;
  device_coords = Many m BE16;
}


-- This type seems to be broken, so we just don't parse it.
-- See: http://www.color.org/PSD_TechNote.pdf
def ProfileSequenceDescType = {
  StartTag "pseq";
}


def ViewConditionsType = {
  StartTag "view";
  illuminantXYZ = XYZNumber;
  surroundXYZ   = XYZNumber;
  illuminant    = BE32;
}


--------------------------------------------------------------------------------
-- Stuff that should be in a library somewhere

def BE16 = UInt8 # UInt8
def BE32 = BE16 # BE16
def BE64 = BE32 # BE32

def getBit n b  = b >> n as! uint 1

-- Goto this offset relative to the current stream
def Goto n = {
  @s = GetStream;
  GotoRel s n;
}

-- Goto this offset relative to the given stream
def GotoRel s n = {
  @s1 = Drop n s;
  SetStream s1;
}

-- Get a chunk of unprocessed bytes
def Chunk sz = {
  @s = GetStream;
  $$ = Take sz s;
  Goto sz;
}

-- Execute a parser and go back to the position before the parser run.
def Remote P = {
  @s = GetStream;
  $$ = P;
  SetStream s;
}

-- Get a chunk relative to the given stream.  Modifies the stream.
def ChunkRelativeTo s off sz = {
  GotoRel s off;
  $$ = Chunk sz;
}

-- Parse a chunk of the given size
def ParseChunk sz P = {
  @s = GetStream;
  @s1 = Take sz s;
  SetStream s1;
  $$ = P;
  @s2 = Drop sz s;
  SetStream s2;
}

def ValidateArray arr P = {
  @s = GetStream;
  SetStream (arrayStream arr);
  P;
  END;
  SetStream s;
}

def Only P = { $$ = P; END }
def exp b e = for (x = 1; i in rangeUp(e)) x * b
def Guard p = p is true
