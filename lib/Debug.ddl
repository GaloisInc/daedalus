def Trace (message : [uint 8]) : {}


def dbgShowDigit x =
  [ if x < 10
      then '0' +  (x as! uint 8)
      else 'A' + ((x as! uint 8) - 10)
  ]

-- This is pretty inefficient but convenient while debugging
def dbgShowNumberBase b x =
  if x < b
    then dbgShowDigit x
    else concat [ dbgShowNumberBase b (x/b), dbgShowDigit (x%b) ]

-- This is pretty inefficient but convenient while debugging
def dbgShowNumber x = dbgShowNumberBase 10 x
def dbgShowHexNumber x = dbgShowNumberBase 16 x

