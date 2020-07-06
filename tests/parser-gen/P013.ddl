{- Tests
   Coerce
-}

def A = { $$ = UInt8 }

def Main = { x = A, a = (^ x as uint 32), b = (^ x as int) }
