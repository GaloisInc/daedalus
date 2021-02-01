def Bool = Choose1 {
  { @'0' ; ^false };
  { @'1' ; ^true };
}

-- TODO: fix
def UInt16 = {
  @hi = UInt8;
  @lo = UInt8;
  ^((hi as uint 16) * 256 + (lo as uint 16))
}

-- TODO: waiting on description from LM
def Int32 = Fail "Int32: not defined"

-- TODO: waiting on description from LM
def Float = Fail "Float: not defined"

