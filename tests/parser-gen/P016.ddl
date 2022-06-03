{- Tests
   for grammar loop
-}
def $digit = '0'..'9'

def A = { a = Many $digit; @b = $['|'] }

def Main = { x = A ;
             y = for (r = 0; b in x.a)
                 { @c = UInt8
                 ; ^ ((c as int) + r)
                 };
             z = UInt8
           }
