-- Ex3.1 from notes.org 

def Main = {
  b = Match1 ('a'..'z');
  x = Match1 ('a'..'z');
  Choose {
    c1 = Match1 ('a'..'z');
    c2 = x < b is true;
  }
}
