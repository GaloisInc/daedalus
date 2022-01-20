-- testing LL(k) with Match and some semantic value recomposition

def Main = { x = Simple1 ; y = Simple2 }

def Simple1 =
    Match "abc"
  | Match "abde"


def Simple2 =
    Match "000 AAAAA"
  | Match "001 BBBBB"
  | Match "010 CCCCC"
  | Match "011 DDDDD"
  | Match "100 EEEEE"
  | Match "101 FFFFF"
  | Match "110 GGGGG"
  | Match "111 HHHHH"
