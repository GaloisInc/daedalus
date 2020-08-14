
def a = { x = true }
def b = { x = true } : a
def c = { x = true }
def d = { x = true } : d
def e = { x = { x = true } : local; y = { x = true } : local }
def f (i : a) = i.x


def A = Choose { x = Match1 1; y = Match1 2 }
def B = Choose { x = Match1 3; y = Match1 4 } : A
def C = Choose { x = Match1 1; y = Match1 2 }
def D = Choose { x = Match1 1; y = Match1 2 } : D
def E = { x = Choose { x = Match1 1; y = Match1 2 } : local;
          y = Choose { x = Match1 1; y = Match1 3 } : local
        }

def F (i : A) = i is x

