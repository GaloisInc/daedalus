def P = Choose1 {
  a = Match1 'a';
  b = Match1 'b';
}

def Main = {
  SetStream (arrayStream ['a','b']);
  x = P;
  y = P;
  lt = x < y;
  eq = x == y;
  neq = x != y;
  gt  = x > y;
  leq = x <= y;
  geq = x >= y;
}
