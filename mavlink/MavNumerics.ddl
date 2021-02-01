def Byte = 0 .. 255

def Bool = Choose1 {
  mavFalse = @0;
  mavTrue = @1;
}

def Uint8 = Byte

def Uint16 = {
  hi = Byte;
  lo = Byte;
  ^(hi * 255 + lo)
}

-- TODO: waiting on description from LM
def Int32 = Fail "Int32: not defined"

-- TODO: waiting on description from LM
def Float = Fail "Float: not defined"

