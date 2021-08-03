def Main = 1 + 2

def f x = x.l


def P x = block
  x as int
  x as? int
  x as! int

def J x y = x # y

def L x =
  block
    for (s = 0; i in x) s + i
    map (i in x) true
