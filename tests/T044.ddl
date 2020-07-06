
def a = { x = true }
def b = { x = true } : a
def c = { x = true }
def d = { x = true } : d
def e = { x = { x = true } : local; y = { x = true } : local }
def f (i : a) = i.x


def A = Choose { x = 1; y = 2 }
def B = Choose { x = 3; y = 4 } : A
def C = Choose { x = 1; y = 2 }
def D = Choose { x = 1; y = 2 } : D
def E = { x = Choose { x = 1; y = 2 } : local;
          y = Choose { x = 1; y = 3 } : local
        }

def F (i : A) = i is x

