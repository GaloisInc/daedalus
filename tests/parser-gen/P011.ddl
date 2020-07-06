{- Tests ShiftR, ShiftL
-}

def Length =
         { @a = UInt8 ;
           @b = UInt8 ;
           @c = UInt8 ;
           @d = UInt8 ;
           ^ (a as int << 24) +
             (b as int << 16) +
             (c as int << 8) +
             (d as int << 0); }

def Main = { x = Length, y = (^ x >> 24), z = UInt8 }
