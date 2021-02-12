-- Zoo7 from notes.org. Appearance in the same expression is not necessarily entanglement

def Main = {
  a = Match1 UInt8;
  b = Match1 UInt8;
  c = ^ a + b;  
  c < 10 is true;
}
