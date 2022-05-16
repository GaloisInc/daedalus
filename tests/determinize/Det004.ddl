-- testing LL(k) with calling grammar some semantic value recomposition

def Main = { x = Gram1 ; y = Gram2, z = Gram3 }

def Gram1 =
     { Match "a"; Gram2}
  <| { Match "a"; Gram3}

def Gram2 =
   $['b']

def Gram3 =
  $['c']

