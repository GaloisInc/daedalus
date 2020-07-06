

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
  "acsp";
  primary_platform    = PrimaryPlatforms;
  profile_flags       = BE32;
  device_manufacturer = BE32;
  device_model        = BE32;
  device_attributes   = BE64;
  rendering_intent    = RenderingIntent;
  illuminant          = XYZNumber;
  creatior            = BE32;
  identifier          = Many 16 UInt8;
  reserved_data       = Many 28 0;
}

def VersionField = {
  major        = UInt8;
  @min_bf      = UInt8;
  minor        = ^ min_bf >> 4 as! uint 4;
  bugfix       = ^ min_bf as! uint 4;
  0x00; 0x00;
}

def ProfileClasses = {
  Choose1 {
    input_device_profile   = @"scnr";
    display_device_profile = @"mntr";
    output_device_profile  = @"prtr";
    device_link_profile    = @"link";
    color_space_profile    = @"spac";
    abstract_profile       = @"abst";
    named_color_profile    = @"nmcl";
  }
}

def DataColorSpaces = {
  Choose1 {
    nciexyz_or_pcsxyz = @"XYZ ";
    cielab_or_pcslab  = @"Lab ";
    cieluv            = @"Luv ";
    ycbcr             = @"Ycbr";
    cieyxy            = @"Yxy ";
    rgb               = @"RGB ";
    gray              = @"GRAY";
    hsv               = @"HSV ";
    hls               = @"HLS ";
    cmyk              = @"CMYK";
    cmy               = @"CMY ";
    two_colour        = @"2CLR";
    three_colour      = @"3CLR";
    four_colour       = @"4CLR";
    five_colour       = @"5CLR";
    six_colour        = @"6CLR";
    seven_colour      = @"7CLR";
    eight_colour      = @"8CLR";
    nine_colour       = @"9CLR";
    ten_colour        = @"ACLR";
    eleven_colour     = @"BCLR";
    twelve_colour     = @"CCLR";
    thirteen_colour   = @"DCLR";
    fourteen_colour   = @"ECLR";
    fifteen_colour    = @"FCLR";
  }
}

def PrimaryPlatforms = {
  Choose1 {
  none                    = [0,0,0,0];
    apple_computer_inc    = @"APPL";
    microsoft_corporation = @"MSFT";
    silicon_graphics_inc  = @"SGI ";
    sun_microsystems      = @"SUNW";
  }
}


def RenderingIntent = {
  Choose1 {
    perceptual                  = [0,0,0,0];
    media_relative_colorimetric = [0,0,0,1];
    saturation                  = [0,0,0,2];
    icc_absolute_colorimetric   = [0,0,0,3];
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
  $$ = Many { @x = (1..); x as uint 7 };
  Many (1 .. ) (0 <| Fail "Non 0 string terminator");
}

def Response16Number = {
  device = BE16;
  Many 2 0;
  measurement = BE32;
}





--------------------------------------------------------------------------------
-- Tag table (Section 7.3)

def TagTable = {
  @tag_count = BE32;
  Many (tag_count as int) TagEntry;
}

def TagEntry = {
  tag_signature           = Many 4 UInt8;
  offset_to_data_element  = BE32;
  size_of_data_element    = BE32;
}

-- ENTRY: Should only be used when the stream is at offset 0
def ParseTag (t : TagEntry) = {
  Goto (t.offset_to_data_element as int);
  ParseChunk (t.size_of_data_element as int) (Tag t.tag_signature);
}



--------------------------------------------------------------------------------
-- Tag Definitions (Section 9)

def Tag sig = Choose1 {
  AToB0               = { sig == "A2B0"; commit; Lut_8_16_AB };
  AToB1               = { sig == "A2B1"; commit; Lut_8_16_AB };
  AToB2               = { sig == "A2B2"; commit; Lut_8_16_AB };
  blueMatrixColumn    = { sig == "bXYZ"; commit; XYZType };
  blueTRC             = { sig == "bTRC"; commit; SomeCurve };
  BToA0               = { sig == "B2A0"; commit; Lut_8_16_BA };
  BToA1               = { sig == "B2A1"; commit; Lut_8_16_BA };
  BToA2               = { sig == "B2A2"; commit; Lut_8_16_BA };
  BToD0               = { sig == "B2D0"; commit; MultiProcessElementsType };
  BToD1               = { sig == "B2D1"; commit; MultiProcessElementsType };
  BToD2               = { sig == "B2D2"; commit; MultiProcessElementsType };
  BToD3               = { sig == "B2D3"; commit; MultiProcessElementsType };
  calibrationDateTime = { sig == "calt"; commit; DateTimeType };
  charTarget          = { sig == "targ"; commit; TextType };
  chromaticAdaptation = { sig == "chad"; commit; S15Fixed16ArrayType };
  colorantOrder       = { sig == "clro"; commit; ColorantOrderType; };
  colorantTable       = { sig == "clrt"; commit; ColorantTableType; };
  colorantTableOut    = { sig == "clot"; commit; ColorantTableType; };
  colorimetricIntentImageState =
                        { sig == "ciis"; commit; SignatureType };
  copyright           = { sig == "cprt"; commit; MultiLocalizedUnicodeType };
  deviceMfgDesc       = { sig == "dmnd"; commit; MultiLocalizedUnicodeType };
  deviceModelDesc     = { sig == "dmdd"; commit; MultiLocalizedUnicodeType };
  DToB0               = { sig == "D2B0"; commit; MultiProcessElementsType };
  DToB1               = { sig == "D2B1"; commit; MultiProcessElementsType };
  DToB2               = { sig == "D2B2"; commit; MultiProcessElementsType };
  DToB3               = { sig == "D2B3"; commit; MultiProcessElementsType };
  gamut               = { sig == "gamt"; commit; Lut_8_16_BA };
  grayTRC             = { sig == "kTRC"; commit; SomeCurve };
  greenMatrixColumn   = { sig == "gXYZ"; commit; XYZType };
  greenTRC            = { sig == "gTRC"; commit; SomeCurve };
  luminance           = { sig == "lumi"; commit; XYZType };
  measurement         = { sig == "meas"; commit; MeasurementType };
  mediaWhitePoint     = { sig == "wtpt"; commit; XYZType };
  namedColor2         = { sig == "ncl2"; commit; NamedColor2Type };
  outputResponse      = { sig == "resp"; commit; ResponseCurveSet16Type };
  perceptualRenderingIntentGamut =
                        { sig == "rig0"; commit; SignatureType };
  preview0            = { sig == "pre0"; commit; Lut_8_16_AB_BA };
  preview1            = { sig == "pre1"; commit; Lut_8_16_BA };
  preview2            = { sig == "pre2"; commit; Lut_8_16_BA };
  profileDescription  = { sig == "desc"; commit; MultiLocalizedUnicodeType };
  profileSequenceDesc = { sig == "pseq"; commit; ProfileSequenceDescType }; -- XXX
  profileSequenceIdentifier =
                        { sig == "psid"; commit; {} }; -- XXX
  redMatrixColumn     = { sig == "rXYZ"; commit; XYZType; };
  redTRC              = { sig == "rTRC"; commit; SomeCurve };
  saturationRenderingIntentGamut = { sig == "rig2"; commit; SignatureType };
  technology          = { sig == "tech"; commit; SignatureType };
  viewCondDesc        = { sig == "vued"; commit; MultiLocalizedUnicodeType };
  viewConditions      = { sig == "view"; commit; ViewConditionsType };
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


def DateTimeType = {
  "dtim";
  commit; Many 4 0;
  DateTimeNumber;
}

def TextType = {
  "text";
  commit; Many 4 0;
  Only ASCII7;
}

def SignatureType = {
  "sig ";
  commit; Many 4 0;
  Many 4 UInt8;
}

def MultiLocalizedUnicodeType = {
  @s = GetStream;   -- Offsets are relative to here
  "mluc";
  commit; Many 4 0;
  @record_number = BE32;
  @record_size   = BE32;
  record_size == 12;
  Many (record_number as int) (UnicodeRecord s);
}

def UnicodeRecord s = {
  language = BE16;
  country  = BE16;
  @size    = BE32;
  @offset  = BE32;
  data     = Remote (ChunkRelativeTo s (offset as int) (size as int));
}

def S15Fixed16ArrayType = {
  "sf32";
  commit; Many 4 0;
  Many BE32;    -- fixed point rationals
}

def ChromaticityType = {
  "chrm";
  commit; Many 4 0;
  @number_of_device_channels = BE16;
  phosphor_or_colorant       = BE16;
  cie_coords                 = Many (number_of_device_channels as int) XYNumber;
}

def ColorantOrderType = {
  "clro";
  commit; Many 4 0;
  @count_of_colorants = BE32;
  Many UInt8;
}

def ColorantTableType = {
  "clrt";
  commit; Many 4 0;
  @count_of_colorant = BE32;
  Many (count_of_colorant as int) Colorant;
}

def Colorant = {
  name = ParseChunk 32 (Only ASCII7);
  pcs  = Many 3 BE16;
}

def CurveType = {
  "curv";
  commit; Many 4 0;
  @n = BE32;
  Many (n as int) BE16;
}

def ParametricCurveType = {
  "para";
  commit; Many 4 0;
  function = BE16;
  Many 2 0;
  parameters = Many BE32;
    -- These are to be interpreted as fixed precision rationals
}

def ResponseCurveSet16Type = {
  @s = GetStream;
  "rcs2";
  commit; Many 4 0;
  @number_of_channels = BE16;
  @count              = BE16;
  Many (count as int) {
    @off  = BE32;
    Remote { GotoRel s (off as int); ResponseCurve (number_of_channels as int)};
  }
}

def ResponseCurve n = {
  measurement_unit  = BE32;
  @counts           = Many n BE32;
  pcxyzs            = Many n XYNumber;
  response_arrays   = map (qi in counts) (Many (qi as int) Response16Number)
}

def Lut8Type = {
  "mft1";
  commit; Many 4 0;
  number_of_input_channels = UInt8;
  @i = ^ number_of_input_channels as int;
  number_of_output_channels = UInt8;
  @o = ^ number_of_output_channels as int;
  number_of_clut_grid_points = UInt8;
  @g = number_of_clut_grid_points as int;
  0x00;
  encoded_e_parameters = Many 9 { @x = BE32; ^ x as! sint 32 };
  input_tables  = Chunk (256 * i);
  clut_values   = Chunk ((exp g i) * o);
  output_tables = Chunk (256 * o);
}

def Lut16Type = {
  "mft2";
  commit; Many 4 0;
  number_of_input_channels = UInt8;
  @i = ^ number_of_input_channels as int;
  number_of_output_channels = UInt8;
  @o = ^ number_of_output_channels as int;
  number_of_clut_grid_points = UInt8;
  @g = number_of_clut_grid_points as int;
  0x00;
  encoded_e_parameters = Many 9 { @x = BE32; ^ x as! sint 32 };
  number_of_input_table_entries = BE32;
  @n = ^ number_of_input_table_entries as int;
  number_of_output_table_entries = BE32;
  @m = ^ number_of_output_table_entries as int;
  input_tables  = Chunk (256 * n * i);
  clut_values   = Chunk (2 * (exp g i) * o);
  output_tables = Chunk (2 * m * o);
}

def LutAToBType = {
  "mAB ";
  commit; Many 4 0;
  number_of_input_channels  = UInt8;
  number_of_output_channels = UInt8;
  Many 2 0;
  offset_first_B_curve      = BE32;
  offset_to_matrix          = BE32;
  offset_to_first_M_curve   = BE32;
  offset_to_CLUT            = BE32;
  offset_to_first_A_curve   = BE32;
  data                      = GetStream;
}

-- XXX: Why is this the same as the AB case?
def LutBToAType = {
  "mBA ";
  commit; Many 4 0;
  number_of_input_channels  = UInt8;
  number_of_output_channels = UInt8;
  Many 2 0;
  offset_first_B_curve      = BE32;
  offset_to_matrix          = BE32;
  offset_to_first_M_curve   = BE32;
  offset_to_CLUT            = BE32;
  offset_to_first_A_curve   = BE32;
  data                      = GetStream;
}


def MultiProcessElementsType = {
  @s = GetStream;   -- offsets are relative to here
  "mpet";
  commit; Many 4 0;
  number_of_input_channels      = BE16;
  number_of_output_channels     = BE16;
  number_of_processing_elements = BE32;
  n = ^ number_of_processing_elements as int;
  n > 0;
  @els = Many n PositionNumber;
  elements = map (e in els)
                 (ChunkRelativeTo s (e.offset as int) (e.size as int));
}


-- XXX: Shall we reqiure that there are no left over bytes after the XYZ number?
def XYZType = {
  "XYZ ";
  commit; Many 4 0;
  Many XYZNumber;
}


-- XXX: the values can be parsed a bit more.
def MeasurementType = {
  "meas";
  commit; Many 4 0;
  standard_observer = BE32;
  nCIEXYZ           = XYZNumber;
  geometry          = BE32;
  flare             = BE32;
  illuminant        = BE32;
}


def NamedColor2Type = {
  "ncl2";
  commit; Many 4 0;
  vendor_specific   = BE32;
  @count            = BE32;
  @number_of_coords = BE32;
  prefix            = ParseChunk 32 (Only ASCII7);
  suffix            = ParseChunk 32 (Only ASCII7);
  names             = Many (count as int) (ColorName (number_of_coords as int));
}

def ColorName m = {
  name_root     = ParseChunk 32 ASCII7;
  pcs_coords    = Many 3 BE16;
  device_coords = Many m BE16;
}


-- This type seems to be broken, so we just don't parse it.
-- See: http://www.color.org/PSD_TechNote.pdf
def ProfileSequenceDescType = {
  "pseq";
  commit; Many 4 0;
  {}
}


def ViewConditionsType = {
  "view";
  commit; Many 4 0;
  illuminantXYZ = XYZNumber;
  surroundXYZ   = XYZNumber;
  illuminant    = BE32;
}


--------------------------------------------------------------------------------
-- Stuff that should be in a library somewhere

def BE16 = {
  @u = UInt8;
  @l = UInt8;
  ^ u # l;
}

def BE32 = {
  @u = BE16;
  @l = BE16;
  ^ u # l;
}

def BE64 = {
  @u = BE32;
  @l = BE32;
  ^ u # l;
}

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
