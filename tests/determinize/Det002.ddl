-- testing LL(1) with Match and some semantic value

def Main = { x = Simple1 ; y = Simple2 }

def Simple1 =
     {@x = Match "ab"; ^ concat [ x, x ]}
  <| Match "bcde"

def Simple2 =
     Match "abc"
  <| Match "bc"
  <| { @x = $['c'] ; @y = Match "b"; ^ (concat [ [x], y]) }


