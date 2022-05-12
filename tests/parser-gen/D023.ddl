-- Test the data-dependent stream operation in an unambiguous setting

def Main =
  block
    let x = UInt8 as uint 64
    y = GetStream
    SetStream (Take x y)
    Many x $['a']
    SetStream (Drop x y)
    END
