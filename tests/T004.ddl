
def Repeat P =
  First
    cons = block
             head = P
             tail = Repeat P
    nil  = Accept

