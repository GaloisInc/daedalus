

def Main = {
  Match1 '+' | Match1 '-';
  Many F;
}

def F = Match1 'a' | Match1 'b'


