def Bool = Choose1 {
  { @'0' ; ^false };
  { @'1' ; ^true };
}

-- UInt16: two-byte unsigned integer
def UInt16 = {
  @hi = UInt8;
  @lo = UInt8;
  ^(256 * (hi as uint 16) + (lo as uint 16))
}

-- TODO: waiting on description from LM
def Int32 = UInt8

-- TODO: waiting on description from LM
def Float = UInt8

