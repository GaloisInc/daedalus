import Daedalus

def Main =
  block
    ex1 = Check 0
    ex2 = Check ((1 : uint 1) # 0)

def Check xs =
  block
    value = wordToFloat xs
    isNan = isNaN value
    isNZ  = isNegativeZero value
    isInf = isInfinite value
    isDen = isDenormalized value



