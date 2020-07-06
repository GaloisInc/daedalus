def IsValidated     (obj : int) (gen : int) (ty : [uint 8]) : bool
def StartValidating (obj : int) (gen : int) (ty : [uint 8]) : {}

def CheckInteger (v : Value) : int =
  { @n = v is number; n.exp == 0; ^ n.num }
  -- XXX: +ve exponents are OK, as are -ve if number is big enough

-- XXX: more checking
def CheckDate (v : Value) : [uint 8] =
  { v is string }

-- XXX: more checking
def CheckText (v : Value) : [uint 8] =
  { v is string }

-- XXX: more checking?
def CheckASCII (v : Value) : [uint 8] =
  { v is string }

-- XXX
def CheckRectangle (v : Value) : {} =
  {}

-- XXX 
def EqNumber (x : Number) (y : Number) : {} =
  { x.num == y.num; x.exp == y.exp }
