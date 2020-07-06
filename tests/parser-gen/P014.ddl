{- Tests
   NArray and concat
-}

def A =
    { a = UInt8 ;
      b = UInt8 ;
      c = UInt8 ;
      d = Many UInt8;
    }

def Main = { x = A; y = ^ concat [ x.d, [x.a], [x.c], [x.b] ]; END }
