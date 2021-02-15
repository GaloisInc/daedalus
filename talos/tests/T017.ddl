-- Zoo1 from notes.org: Variables are entangled only on some paths

def Main = {
  x = Match1 ('a'..'z');
  y = Match1 ('a'..'z');
  z = Match1 ('a'..'z');

  Choose {
    c1 = { x < y is true };
    c2 = {};
  };
  Choose {
    c3 = { y < z is true };
    c4 = {}; 
  };
  Choose {
    c5 = { z < x is true };
    c6 = {}; 
  }; 
}
