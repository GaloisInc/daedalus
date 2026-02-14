-- Test: union with 4 alternatives (stress switch codegen).

def Main = First
  circle   = { @tag = UInt8; tag == 1 is true; radius = UInt8 }
  square   = { @tag = UInt8; tag == 2 is true; side = UInt8 }
  triangle = { @tag = UInt8; tag == 3 is true; base = UInt8; height = UInt8 }
  rect     = { @tag = UInt8; tag == 4 is true; w = UInt8; h = UInt8 }
