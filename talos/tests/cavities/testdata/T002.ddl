-- Test that a cavity with a fixed condition is not detected as an unbounded
-- cavity.

def Main =
  block
    -- Not a cavity
    let x = Many UInt8
    x == "START" is true
    -- Cavity
    Many UInt8
