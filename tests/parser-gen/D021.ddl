-- test data dependent instructions on non-deterministic path

def Main =
  block
    let y = UInt8
    let x = UInt8
    t = Many
          (x as uint 64)
          (Many
            (y as uint 64)
            (Match "abc"))
    END
