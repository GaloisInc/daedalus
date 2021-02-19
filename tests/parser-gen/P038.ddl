-- Another test for the Runner with bias and unbias choice

def Block1 =
  { Match "a" }
  <|
  { Match "a" }

def Block2 =
  Block3
  <|
  Match "RANDOM"

-- (acd | ac) d
def Block3 =
  {  { Match "acd"
     | Match "ac"
     }
  ; Match "d"
  }

def Main =
  { Block1
  ; Block2
  ; Match "e"
  ; END
  }
  <|
  { Match "aacdde"; END }
