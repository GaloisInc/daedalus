def Main = block
  let tag = UInt8
  case tag of
    0x01 -> {| A |}
    0x02 -> {| B = Nested |}

def Nested = block
  x = UInt8
  y = UInt8
