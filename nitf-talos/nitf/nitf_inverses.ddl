
def inverse_UnsignedNum (digs : uint 64) (v : uint 64) =
  case digs of {
    0 -> [];
    _ -> concat [ inverse_UnsignedNum (digs - 1) (v / 10), [ (v % 10) as! uint 8 + '0' ] ]
  }

def exp10 (n : uint 64) =
  case n of {
    0 -> (1 : uint 64);
    _ -> 10 * exp10 (n - 1)
  }


def pred_UnsignedNum (digs : uint 64) (v : uint 64) = v < exp10 digs
