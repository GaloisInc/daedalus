-- Zoo2 from notes.org. Path choices requires refining future paths

def Main = {
  a = $['a'..'z'];
  Choose {
    left  = a < 'c' is true;
    right = a >= 'y' is true;
  };
  Choose {
    first  = a < 'e' is true;
    second = a >= 'r' is true; 
  };
}
