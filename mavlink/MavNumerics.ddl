def Guard p = p is true

def Bool = Choose1 {
  { @'0' ; ^false };
  { @'1' ; ^true };
}

-- UInt16: two-byte unsigned integer
def UInt16 = {
  @hi = UInt8;
  @lo = UInt8;
  hi # lo
}

def HighBit = (1 : uint 8) << 7

-- Int8: a signed 8-bit integer, serialized in two's complement
def Int8 = {
  @dig = UInt8;
  (dig .&. ~HighBit as int) - (dig .&. HighBit as int)
}

-- Int24: a signed 24-bit integer, serialized in two's complement
def Int24 = {
  @dig0 = UInt8;
  @dig1 = UInt8;
  @dig2 = UInt8;
  ((dig0 .&. ~HighBit) # dig1 # dig2 as int) -
  ((dig0 .&. HighBit as uint 24) << 16 as int)
}

-- Int32: a signed 32-bit integer, serialized in two's complement
def Int32 = {
  @dig0 = UInt8;
  @dig1 = UInt8;
  @dig2 = UInt8;
  @dig3 = UInt8;
  ((dig0 .&. ~HighBit) # dig1 # dig2 # dig3 as int) -
  ((dig0 .&. HighBit as uint 32) << 24 as int)
  -- TODO: update docs on bitwise operand symbols
}

def Digit = Match1 ('0' .. '9') - '0' as int

def numBase base ds = for (val = 0; d in ds) (val * base + d)

def Natural numDigs = numBase 10 (Many numDigs Digit)

def LowBits = ~HighBit

def IsInfty mant exp = {
  Guard (exp == 0xFF);
  Guard (mant == 0)
}

-- Float: single precision (32-bit) float, serialied in IEEE754
-- TODO: correctly represent sign bit, biased exp, significand, special values
def Float = {
  -- read bytes:
  @dig0 = UInt8;
  @dig1 = UInt8;
  @mantissaLow = UInt16;

  -- shift bits
  @sign = ^(if (dig0 .&. HighBit) then 1 else -1);
  @mant = ^(sign * (((dig1 .&. LowBits) << 16) + mantissaLow));
  @exp = ((dig0 .&. LowBits) << 1) + ((dig1 .&. HighBit) >> 7);

  -- check for special values:
  Choose1 {
    posInfty = {
      IsInfty mant exp;
      Guard (sign == 1)
    };
    negInfty = {
      IsInfty mant exp;
      Guard (sign == -1)
    };
    nan = Guard (exp == 0xFF);
    number = {
      mantissa = ^(sign * mant);
      exponent = ^exp;
    };
  }
}

def Sub60 = {
  $$ = Natural 2;
  Guard (0 <= $$);
  Guard ($$ <= 59)
}

-- GeoCoord: a geographic coordinate
def GeoCoord = {
  degs = Natural 3;
  Match1 '.';
  mins = Sub60;
  secs = Sub60;
  decSecs = Natural 2;
}

-- Lat: latitude
def Lat = GeoCoord

def Long = GeoCoord
