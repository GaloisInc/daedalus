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

-- Int32: a signed 32-bit integer, serialized in two's complement
def Int32 = {
  @dig0 = UInt8;
  @dig1 = UInt8;
  @dig2 = UInt8;
  @dig3 = UInt8;
  (dig0 # dig1 # dig2 # dig3 as int) -
  ((dig0 .&. 128 as uint 32) << 24 as int) -- TODO: update docs on bitwise ops
}

def Digit = '0' .. '9'

def numBase base ds = for (val = 0; d in ds) (val * base + d)

def Natural numDigs = numBase 10 (Many numDigs Digit)

-- TODO: waiting on description from LM
def Float = UInt8

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
