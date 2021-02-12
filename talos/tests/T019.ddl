-- Zoo3 from notes.org. Path choices requires refining future paths

def Main = {
  r = Choose {
    left  = { 
      x = Match1 ('a'..'z'); 
      y = Match1 ('a'..'z'); 
    };
    right = { a = Match1 ('a'..'z') };
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
