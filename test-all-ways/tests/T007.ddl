def Main =
  block
    SetStream (arrayStream "abc")
    let b = many (s = { count = 0 : int, vals = builder })
                if (s.count < 3)
                   then block
                          count = s.count + 1
                          vals  = emit s.vals UInt8
                   else Fail "DONE"
    build b.vals
