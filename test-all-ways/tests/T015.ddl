def Value =
  First
    array = block
      Match "["
      $$ = Many Value
      Match "]"
    byte = UInt8

def Main =
  block
    SetStream (arrayStream "x")
    let value = Value
    value is byte
