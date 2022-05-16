-- Zoo3 from notes.org. Path choices requires refining future paths

def Main = {
  r = Choose {
    left  = { 
      x = $['a'..'z']; 
      y = $['a'..'z']; 
    };
    right = { a = $['a'..'z'] };
  };
  Choose {
    first  = { 
      v = r is left; 
      ^ v.x 
    };
    second = {
      v = r is right; 
      ^ v.a 
    };
  };
}
