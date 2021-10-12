
def a = { x = true }
def b = { x = true } : a
def c = { x = true }
def d = { x = true } : d
def e = { x = { x = true } : ?local; y = { x = true } : ?local }
def f (i : a) = i.x


def A =
  First
    x = $[1]
    y = $[2]

def B =
  First
    x = $[3]
    y = $[4]
  : A

def C =
  First
    x = $[1]
    y = $[2]

def D =
  First
    x = $[1]
    y = $[2]
  : D

def E =
  First
    x = First
          x = Match1 1
          y = Match1 2
        : ?local
    y = First
          x = Match1 1
          y = Match1 3
        : ?local

def F (i : A) = i is x

