
def l1 xs = for (s = 0 : int; x in xs) s + x
def l2 xs = for (s = 0 : int; k,x in xs) s + x + (k as int)

def ExampleMap = for (s = empty; k,x in [ 5 : int, 6, 7 ]) (Insert k x s)

def Main = {
  a1 = ^ l1 [1,2,3];
  a2 = ^ l2 [1,2,3];
  m  = ExampleMap;
  a3 = ^ l1 m;
  a4 = ^ l2 m;
}



