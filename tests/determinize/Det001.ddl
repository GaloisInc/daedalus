-- Testing LL(1) with Match1 and some semantic value

def Main = { x = Simple1 ; y = SimpleNot1; z = Simple2; t = Simple3 }

def Simple1 =
    Match1 'a'
  | Match1 'b'
  | Match1 'c'
  | Match1 'd'

def SimpleNot1 =
    Match1 'e'
  | Match1 'f'
  | Match1 'f'
  | Match1 'g'

def Simple2 =
     @Match1 'A'
  <| @Match1 'B'
  <| @Match1 'C'

def Simple3 =
     { @x = Match1 'a' ; @Match1 'b'; ^ x }
  <| Match1 'b'
  <| { @x = Match1 'c' ; @Match1 'b'; ^ x + x }

