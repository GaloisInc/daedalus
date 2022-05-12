-- Testing LL(1) with $[_] and some semantic value

def Main = { x = Simple1 ; y = SimpleNot1; z = Simple2; t = Simple3 }

def Simple1 =
    $['a']
  | $['b']
  | $['c']
  | $['d']

def SimpleNot1 =
    $['e']
  | $['f']
  | $['f']
  | $['g']

def Simple2 =
     @$['A']
  <| @$['B']
  <| @$['C']

def Simple3 =
     { @x = $['a'] ; @$['b']; ^ x }
  <| $['b']
  <| { @x = $['c'] ; @$['b']; ^ x + x }

