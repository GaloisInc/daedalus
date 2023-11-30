-- Test that a recursive loop with a builder and a guard is detected as a
-- cavity.

def Main = 
  block
    RecLoop builder

def RecLoop bldr =
  First
    Match "END"
    block
      -- Cavity
      let c = UInt8
      RecLoop (emit bldr c)
