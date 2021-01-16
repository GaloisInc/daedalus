{- Tests Bitwise operator, or, and, xor
-}

def Length =
         { a = UInt8,
           b = UInt8,
           c = UInt8,
           d = UInt8,
           e = UInt8,
           f = UInt8,
         }

def Main =
       { x = Length,
         vor =  ^ (x.a as uint 8) .|. (x.b as uint 8),
         vand = ^ (x.c as uint 8) .&. (x.d as uint 8),
         vxor = ^ (x.e as uint 8) .^. (x.f as uint 8),
       }
