{- Tests
   fun call
-}


def myfun (m: int) =
  (mygauss m) + 42

def mygauss (m : int) =
  if m == 0 then 0 else (m + mygauss (m - 1))

def A = { $$ = UInt8 }

def Main = { x = A, y = (^ myfun (x as int)) }
