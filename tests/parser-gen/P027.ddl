
def Frac =
       { @dot = Match "."
       ; @t = { @ds = Many Digit; ^ numBase 10 ds }
       ; ^ t
       }

def NoFrac = { @ds = Match "" ; ^ -42 }

def Continue = ( Frac <| NoFrac )

def UnsignedNumber = Choose{
                   { n = Natural
                   ; y = Continue
                   }
                 ;
                   { @n = Match ""
                   ; n = ^ 0
                   ; @x = $['.']
                   ; y = Natural
                   }
                 }

def numBase base ds       = for (val = 0; d in ds) (val * base + d)

def Natural = { @ds = Many (1..) Digit; ^ numBase 10 ds }

def Digit     = { @d = $['0' .. '9']; ^ d - '0' as int }

def Main = { x = Many { UnsignedNumber; Match " "} ;  }
