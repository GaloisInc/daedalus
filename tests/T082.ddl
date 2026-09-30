def Main = Dispatch 0

def Dispatch (i : uint 64) : {} =
  block
    Operator (i == 0 is true)
    Dispatch (i + 1)

def Operator P =
  First
    P
    Accept
