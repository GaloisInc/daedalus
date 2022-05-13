-- Ex3 from notes.org

def Main = {
  b = $['a'..'z'];
  res = Choose {
    c1 = $['a'..'z'];
    c2 = { 
      x = $['a'..'z'];
      x < b is true; 
    };
  }
}
