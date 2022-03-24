def Trace (message : [uint 8]) : {}


def dbgShowDigit x = ['0' + (x as! uint 8)]

-- This is pretty inefficient but convenient while debugging
def dbgShowNumber x =
  if x < 10
    then dbgShowDigit x
    else concat [ dbgShowNumber (x/10), dbgShowDigit (x%10) ]

