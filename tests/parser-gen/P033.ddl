{- Test variable shadowing, hiding, -}

def $digit = '0'..'9'

def f (x : int) = x as uint8

def A = { @bef = Match1 '<'; a = Many $digit; @after = Match1 '>' }

{-
def B x =
  { $$ = for (r = 0; t in x.a)
         { @c = UInt8
         ; ^ ((c as int) + r + (t as int))
         }
  }
-}

def Main = { x = A ;
             y = -- B x;
                 for (r = 0; t in x.a)
                 { @c = UInt8
                 ; ^ ((c as int) + r + (t as int))
                 };
             t = ^ f (1) ;
             v = A ;
             w = -- B x;
                 for (r = 0; t in v.a)
                 { @c = UInt8
                 ; ^ ((c as int) + r + (t as int))
                 };
             END
           }
