def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def Only P                = { $$ = P; END }
def When P x              = { P; ^ x }
def Guard p               = p is true

def append x y = concat [ x, y ]

def cons x xs = append [ x ] xs

def snoc x xs = append xs [ x ]

-- types of bounded sequences of bytes
def Bytes1 = UInt8

def Bytes2 = {
  second = UInt8;
  rest1 = Bytes1;
}

def Bytes3 = {
  third = UInt8;
  rest2 = Bytes2;
}

def Bytes4 = {
  fourth = UInt8;
  rest3 = Bytes3;
}

-- functions for serializing bounded structures into arrays
def bndBytes1 bs1 = [ bs1 ]

def bndBytes2 bs2 = cons bs2.second (bndBytes1 bs2.rest1)

def bndBytes3 bs3 = cons bs3.third (bndBytes2 bs3.rest2)

def bndBytes4 bs4 = cons bs4.fourth (bndBytes3 bs4.rest3)

def WithStream s P = {
  @cur = GetStream;
  SetStream s;
  $$ = P;
  SetStream cur;
}
