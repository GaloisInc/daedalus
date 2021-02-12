-- Ex3 from notes.org

def Main = {
  b = Match1 ('a'..'z');
  res = Choose {
    c1 = Match1 ('a'..'z');
    c2 = { 
      x = Match1 ('a'..'z');
      x < b is true; 
    };
  }
}
