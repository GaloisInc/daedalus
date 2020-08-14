{- Tests
   MapInsert, MapLookup
-}

def Word = { $$ = Many (Match1 ('a'..'z')) ; @b = Match1 ' ' ; }

def A = { lst = Many Word; }

def Main = {
         x = A ;
         y = (for (r = empty; s in x.lst)
                 {Insert s s r}) ;
         z = Lookup "abc" y ;
         END
       }
