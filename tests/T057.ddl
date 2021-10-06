def I P   = P
def F x y =
  block
    I (Match x)
    @(I (Match y))

def Main = F "[" "]"



