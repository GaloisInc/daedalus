def Main =
  block
    SetStream (arrayStream [1,1,0,1,0])
    one = Example
    SetStream (arrayStream [0,1,0,1,0])
    two = Example

def Example =
  block
    let ?be = UInt8 == 0
    x = Word16
    y = Word16


def Word16 =
  block
    let b1 = UInt8
    let b2 = UInt8
    if ?be then b1 # b2 else b2 # b1

