def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def Only P                = { $$ = P; END }
def When P x              = { P; ^ x }
def Guard p               = p is true

def Chunk n P =  {
  @cur  = GetStream;
  @this = Take n cur;
  @next = Drop n cur;
  SetStream this;
  $$ = P;
  SetStream next;
}

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
    $$ = Many (len as int - 1) UInt8;
    Match1 0x0; -- String terminator
}
def WString = {
    @PadTo 4;
    @len = Uint32; -- Note: length does NOT includes string terminator byte for Wstring
    $$ = Many (len as int) Uint16;
}

-- Sequences of generic type. max can be zero for unbounded sequences
def Sequence P max = {
    @PadTo 4;
    @len = Uint32;
    ( (max > 0) && (len > max)) is false;   -- Fail if seq. exceed max size
    $$ = Many (len as int) P;
}


