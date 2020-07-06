{- Tests
   MapInsert, MapLookup
-}

def Word = { $$ = Many ('a'..'z') ; @b = ' ' ; }

def A = { lst = Many Word; }

def Main = {
         x = A ;
         y = (for (r = empty; s in x.lst)
                 {Insert s s r}) ;
         z = Lookup "abc" y ;
         END
       }
