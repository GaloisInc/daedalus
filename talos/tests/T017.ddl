-- Zoo1 from notes.org: Variables are entangled only on some paths

def Main = {
  x = $['a'..'z'];
  y = $['a'..'z'];
  z = $['a'..'z'];

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
