def defaultEmptyArr aopt = case aopt of {
  just x -> x;
  nothing -> [ ];
}

-- standard library for arrays
def LiftPToArray P = {
  @x = P;
  ^[ x ]
}

def member x xs = for (isMem = false; y in xs) (isMem || x == y)
