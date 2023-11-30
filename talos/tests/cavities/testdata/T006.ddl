-- Test that a cavity with a fixed condition is not detected as an unbounded
-- cavity.

def Main =
  block
    -- Not a cavity
    let x = build (UpTo (Match "\n") builder)
    x == "START" is true
    -- Cavity
    Many UInt8

def UpTo P bldr =
  First
    block
      P
      bldr
    block
      UpTo P (emit bldr UInt8)
