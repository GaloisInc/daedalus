
def f x y = x + y
def g x = x.l

def Fails = Choose {}

def P = Choose { A = Fails
               ; B = Match "B"
               ; C = Match "C"
               }

