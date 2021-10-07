

def P =
  block
    let x = UInt8
    ^ x as int

def Num =
  block
    let ds = Many (1..) ('0' .. '9')
    ^ for (val = 0; d in ds) (val * 10 + (d as int))



