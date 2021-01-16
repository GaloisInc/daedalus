

def V = Choose {
  number = 1 as int;
  other  = Match "X";
}


def LookupInt (arr : [V]) i = Default 0 {
  @n = Index arr i;
  commit;
  n is number;
}

def arr : [V] =
  [ {| number = 1 |}, {| other = "Y" |} ]

def Main = {
  x1 = LookupInt arr 0;
  -- x11 = LookupInt arr 1; -- parse error
  x2 = LookupInt arr 2;
}

def Default x P = P <| ^ x
