
def xs = [7,8,9 : int]

def Mp = for (s = empty; k,x in xs) Insert x k s

def Main = {
  a = ^ map (x in xs) x + 1;
  b = ^ map (k,x in xs) { k = k; x = x + 1 };
  c = map (x in xs) ^ x + 1;
  d = map (k,x in xs) ^ { k = k; x = x + 1 };
  m = Mp;
  e = ^ map (x in m) x + 1;
  f = ^ map (k,x in m) { k = k; x = x + 1 };
  g = map (x in m) ^ x + 1;
  h = map (k,x in m) ^ { k = k; x = x + 1 };
}

