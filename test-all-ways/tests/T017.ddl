def GetKey (x : maybe (int, int)) : int =
  case x of
    just p  -> p.0
    nothing -> 0 : int

def GetValue (x : maybe (int, int)) : int =
  case x of
    just p  -> p.1
    nothing -> 0 : int

def Main =
  block
    pair = (10 : int, 20 : int)
    triple = (1 : int, 2 : int, 3 : int)

    m = insert 10 100 (insert 20 200 (insert 30 300 empty))
    below = lookupLE 5 m
    exact = lookupLE 20 m
    between = lookupLE 25 m
    above = lookupLE 40 m

    pairFirst = pair.0
    pairSecond = pair.1
    tripleFirst = triple.0
    tripleLast = triple.2

    belowIsNothing = below is nothing
    exactIsJust = exact is just
    betweenKey = GetKey between
    betweenValue = GetValue between
    aboveKey = GetKey above
    aboveValue = GetValue above
