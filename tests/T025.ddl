-- Test maps

def Main =
  block
    let k = UInt8
    let v = UInt8
    r     = Insert k v empty
    vr    = Lookup 'x' r
