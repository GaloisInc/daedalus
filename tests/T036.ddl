
def Main =
  block
    let b = ^ 255 : uint 8
    bit7  = ^ b as! uint 7
    bit14 = ^ bit7 # bit7
    bit32 = ^ (bit7 as uint 32) <# bit7
    ans1  = ^ bit32 >> 7
    ans2  = ^ bit32 << 7
    ans3  = ^ testBit ans2 0
    ans4  = ^ testBit ans2 7

def testBit a x = a >> x as! uint 1 == 1
