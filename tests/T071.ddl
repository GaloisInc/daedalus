bitdata B where
  A = 0 : uint 2
  B = 1 : uint 2

bitdata C where
  X = { tag : B, val : uint 6 }
  Y = { 1 : uint 1, val : uint 7 }

def Main = block
  x = 0 as C
  y = 0 as? B

