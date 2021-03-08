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

def LowBits = {
  @hb = HighBit;
  ^ ~hb
}

-- Float: single precision (32-bit) float, serialied in IEEE754
def Float = {
  -- read bytes:
  @dig0 = UInt8;
  @dig1 = UInt8;
  @mantissaLow = UInt16;

  -- define sign bit, mantissa, and exponent:
  @hb = HighBit;
  @signBit = ^(dig0 .&. hb);
  @isPos = Choose1 {
    pos = signBit == 0;
    neg = ^{};
  };
  @lbs = LowBits;
  @mant = ^(((dig1 .&. lbs as uint 23) << 16) .|. (mantissaLow as uint 23));
  @exp = ^(((dig0 .&. lbs) << 1) .|. ((dig1 .&. hb) >> 7));

  -- check for special values:
  Choose1 {
    zero = {
      Guard (exp == 0);
      Guard (mant == 0)
    };
    denormalized = {
      Guard (exp == 0);
      ^ mant
    };
    infty = {
      Guard (exp == 0xFF);
      Guard (mant == 0);
      ^isPos
    };
    nan = {
      Guard (exp == 0xFF);
      Guard (mant != 0)
    };
    number = {
      sign = isPos;
      mantissa = ^mant;
      exponent = ^((exp as int) - 127);
    };
  }
}

-- NonNegFloat f: coerce f into a non-negative number:
def NonNegFloat (f : Float) = Choose1 {
  nonNegZero = f is zero;
  nonNegDenorm = f is denormalized;
  nonNegInfty = {
    @s = f is infty;
    s is pos
  };
  nonNegNumber = {
    @n = f is number;
    n.sign is pos;
    nnMantissa = ^n.mantissa;
    nnExponent = ^n.exponent;
  };
}

-- InclusiveFractional f: coerce f into a value in 0 <= n <= 1
def InclusiveFractional f = {
  @nnf = NonNegFloat f;
  Choose1 {
    inclusiveZero = nnf is nonNegZero;
    inclusiveDenorm = nnf is nonNegDenorm;
    inclusiveFrac = {
      @nnNum = nnf is nonNegNumber;
      Guard (nnNum.nnExponent < 0);
      incMantissa = nnNum.nnMantissa;
      incExponent = nnNum.nnExponent;
    };
    inclusiveOne = {
      @n = nnf is nonNegNumber;
      Guard (n.nnMantissa == 0);
      Guard (n.nnExponent == 0)
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
