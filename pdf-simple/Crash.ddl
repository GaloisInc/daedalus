

def Main = {
  Match1 '+' | Match1 '-';
  Loop;
}

def Loop = { F; Loop } <| ^ {}

def F = Match1 'a' | Match1 'b'


