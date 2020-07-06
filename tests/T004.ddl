
def Repeat P = Choose {
  cons = { head = P ; tail = Repeat P };
  nil  = {}
}

