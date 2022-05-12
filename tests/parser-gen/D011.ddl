-- test Many with constant bound

def numBase (base : int) (ds : [ int ]) =
  for (val = 0; d in ds)
    (val * base + d)

def Numeral = $['0' .. '9']

def Digit = { @d = Numeral ; ^ d - '0' as int }

def UnsignedNum digs = {
  @ds = Many digs Digit ;
  ^ numBase 10 ds
}


def FL = First
  unknown_length = Many 12 $['9']
  -- TODO put interval bounds here:
  file_length = UnsignedNum 12


def Main =
  { b1 = FL
  ; b2 = UnsignedNum 6
  }
