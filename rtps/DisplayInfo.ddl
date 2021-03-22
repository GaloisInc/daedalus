-- Automatically generated - do not edit

--
-- UTILITY PARSERS
def PadTo n = {
    @here = Offset;
    { (here % n == 0) is true; ^ {} } <| @(Many (n - (here % n))  UInt8);
}

-- *** LITTLE ENDIAN ***
def LE16 = { @b1 = UInt8; @b2 = UInt8; ^ b2 # b1 }
def LE32 = { @w1 = LE16;  @w2 = LE16;  ^ w2 # w1 }
def LE64 = { @w1 = LE32;  @w2 = LE32;  ^ w2 # w1 }
def HighBit = (1 : uint 8) << 7

--
-- BASIC TYPES
--

def Boolean = UInt8 > 0

-- Uint8 is natively supported by Daedalus
def Int8 = {
  @dig = UInt8;
  (dig .&. ~HighBit as int) - (dig .&. HighBit as int)
}

def Uint16 = LE16
def Int16 = {
  @dig1 = UInt8;
  @dig0 = UInt8;
  ((dig0 .&. ~HighBit) # dig1 as int) -
  ((dig0 .&. HighBit as uint 16) << 8 as int)
}

def Uint32 = LE32
def Int32 = { 
  @dig3 = UInt8;
  @dig2 = UInt8;
  @dig1 = UInt8;
  @dig0 = UInt8;
  ((dig0 .&. ~HighBit) # dig1 # dig2 # dig3 as int) -
  ((dig0 .&. HighBit as uint 32) << 24 as int)
}

def Uint64 = LE64
def Int64 = { 
  @dig7 = UInt8;
  @dig6 = UInt8;
  @dig5 = UInt8;
  @dig4 = UInt8;
  @dig3 = UInt8;
  @dig2 = UInt8;
  @dig1 = UInt8;
  @dig0 = UInt8;
  ((dig0 .&. ~HighBit) # dig1 # dig2 # dig3 # dig4 #dig5 #dig6 #dig7 as int) -
  ((dig0 .&. HighBit as uint 64) << 56 as int)
}

def Char = UInt8

-- Unsupported / To review
def WChar = LE16
def Float = LE32
def Double = LE64
def LongDouble = { @w1 = LE64;  @w2 = LE64;  ^ w1 } -- Not fully supported


def String = {
    @PadTo 4;
    @len = Uint32; -- Note: length includes string terminator byte
    $$ = Many (len as uint 64 - 1) UInt8;
    Match1 0x0; -- String terminator
}
def WString = {
    @PadTo 4;
    @len = Uint32; -- Note: length does NOT includes string terminator byte for Wstring
    $$ = Many (len as uint 64) Uint16;
}

-- Sequences of generic type. max can be zero for unbounded sequences
def Sequence P max = {
    @PadTo 4;
    @len = Uint32;
    ( (max > 0) && (len > max)) is false;   -- Fail if seq. exceed max size
    $$ = Many (len as uint 64) P;
}


--enum eImageType {
--  @value(-10)
--  JPEG,
--  @value(4)
--  GIF,
--  @value(190)
--  PNG,
--  MPEG4
--};
def Enum_DisplayInfoData_eImageType = {
  @PadTo 4;
  @val = Int32;
  ( (val != -10) && (val != 4) && (val != 190) && (val != 191) ) is false;
  ^ val
}

--enum eTransitionType {
--  NO_TRANSITION,
--  FADE,
--  WIPE_LEFT,
--  WIPE_RIGHT,
--  WIPE_UP,
--  WIPE_DOWN
--};
def Enum_DisplayInfoData_eTransitionType = {
  @PadTo 4;
  @val = Int32;
  ( (val != 0) && (val != 1) && (val != 2) && (val != 3) && (val != 4) && (val != 5) ) is false;
  ^ val
}

--enum eEffectType {
--  NO_EFFECT,
--  BLACK_AND_WHITE,
--  SEPIA,
--  VIVID,
--  SOFTEN
--};
def Enum_DisplayInfoData_eEffectType = {
  @PadTo 4;
  @val = Int32;
  ( (val != 0) && (val != 1) && (val != 2) && (val != 3) && (val != 4) ) is false;
  ^ val
}

--struct DisplayInfo {
--  @key
--  int32 key_id;
--  float background_color_r;
--  float background_color_g;
--  float background_color_b;
--  DisplayInfoData::eImageType image_type;
--  float display_duration;
--  DisplayInfoData::eTransitionType transition_type;
--  DisplayInfoData::eEffectType effect_type;
--};
def DisplayInfoData_DisplayInfo = {
  @PadTo 4;
  key_id = Int32;
  @PadTo 4;
  background_color_r = Float;
  @PadTo 4;
  background_color_g = Float;
  @PadTo 4;
  background_color_b = Float;
  image_type = Enum_DisplayInfoData_eImageType;
  @PadTo 4;
  display_duration = Float;
  transition_type = Enum_DisplayInfoData_eTransitionType;
  effect_type = Enum_DisplayInfoData_eEffectType;
}

-- Instantiable, top-level includes CDR encapsulation signature
def DisplayInfoData_DisplayInfo_Top = {
    Match [0x0, 0x1, 0x0, 0x0];
    $$ = Many DisplayInfoData_DisplayInfo;
}




