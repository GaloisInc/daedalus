-- Test that a recursive loop with a guard is detected as a cavity.

def Main = RecLoop

def RecLoop =
  First
    block
      Match "END"
      ^ ""
    block
      -- Cavity
      UInt8
      RecLoop
