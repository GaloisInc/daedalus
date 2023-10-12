-- Test that a recursive loop is detected as a cavity.

def Main = 
  block
    Match "START"
    build (RecLoop builder)

def RecLoop bldr =
  First
    block
      END
      ^ bldr
    block
      -- Cavity
      let c = UInt8
      RecLoop (emit bldr c)
