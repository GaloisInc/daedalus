
def f x y = x + y
def g x = x.l

def G x = x is l

def Fails = Choose {}

def P = Choose { A = Fails
               ; B = "B"
               ; C = "C"
               }

