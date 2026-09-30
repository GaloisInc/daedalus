def Main = RecursiveDispatch

def RecursiveDispatch =
  First
    RecursiveOperator $['a']
    RecursiveOperator $['b']
    END

def RecursiveOperator P =
  block
    P
    RecursiveDispatch
