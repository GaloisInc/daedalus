{- Tests
   MapInsert, MapLookup -}

def length1 (w: [uint 8]) : int = for (r = 0; s in w) (r + 1)

def Word = { $$ = Many ('a'..'z') ;
             @b = ' ' ;
           }

def Assert p = p is true

def NotAssert p = p is false

def Main = {
  w1 = Word;
  w2 = Word;
  "||| " ;
  Assert (length1 w1 == length1 w2);
  w3 = Word;
  w4 = Word;
  NotAssert (length1 w3 == length1 w4);
}

