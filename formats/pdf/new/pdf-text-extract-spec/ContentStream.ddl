import PdfValue
import PdfDecl
import StandardEncodings
import CMap
import Debug
import Daedalus


def ContentStream (vr : Value) =
  block

    let bytes = Optional
                case vr of
                  ref r ->
                    case ResolveDeclRef r of
                      value v  -> StreamFromArray (v is array)
                      stream s -> s.body is ok
                  array xs -> StreamFromArray xs

    data = case bytes of
             just s ->
                block
                  SetStream s
                  ManyWS
                  Many ContentStreamEntry
             nothing ->
               if ?strict then Fail "Failed to decode bage bytes"
                          else []

    UNPARSED = if ?strict then { END; [] } else Many UInt8

def buildStream b = arrayStream (build b)

def StreamFromArray (xs : [Value]) =
  buildStream block
    for (b = builder; x in xs)
      emit (emitArray b (bytesOfStream (ContentStreamBytes (x is ref)))) ' '

def ContentStreamBytes (r : Ref) : stream = (ResolveStreamRef r).body is ok


--------------------------------------------------------------------------------
-- Content Steram instrutctions

def ContentStreamEntry =
  First
    value    = Value
    operator = ContentStreamOperator

def ContentStreamOperator =
  Token
  First
    -- note that the lengths here are important!

    -- 3
    BDC     = @Match "BDC"
    BMC     = @Match "BMC"
    SCN     = @Match "SCN"
    scn     = @Match "scn"
    EMC     = @Match "EMC"

    -- 2
    b_star  = @Match "b*"
    B_Star  = @Match "B*"

    BT      = @Match "BT"
    ET      = @Match "ET"


    BX      = @Match "BX"
    cm      = @Match "cm"
    CS      = @Match "CS"
    d0      = @Match "d0"
    d1      = @Match "d1"
    Do      = @Match "Do"
    DP      = @Match "DP"
    EX      = @Match "EX"
    f_star  = @Match "f*"
    gs      = @Match "gs"

    BI      = @Match "BI"
    ID      = block
                $$ = Match "ID"
                SkipImageData -- This consumes EI

    MP      = @Match "MP"
    re      = @Match "re"
    RG      = @Match "RG"
    rg      = @Match "rg"
    ri      = @Match "ri"
    SC      = @Match "SC"
    sc      = @Match "sc"
    sh      = @Match "sh"
    T_star  = @Match "T*"
    Tc      = @Match "Tc"
    Td      = @Match "Td"
    TD      = @Match "TD"
    Tf      = @Match "Tf"
    Tj      = @Match "Tj"
    TJ      = @Match "TJ"
    TL      = @Match "TL"
    Tm      = @Match "Tm"
    Tr      = @Match "Tr"
    Ts      = @Match "Ts"
    Tw      = @Match "Tw"
    Tz      = @Match "Tz"
    W_star  = @Match "W*"

    -- 1
    b       = @Match "b"
    B       = @Match "B"
    c       = @Match "c"
    d       = @Match "d"
    f       = @Match "f"
    F       = @Match "F"
    G       = @Match "G"
    g       = @Match "g"
    h       = @Match "h"
    i       = @Match "i"
    j       = @Match "j"
    J       = @Match "J"
    K       = @Match "K"
    k       = @Match "k"
    l       = @Match "l"
    m       = @Match "m"
    M       = @Match "M"
    n       = @Match "n"
    q       = @Match "q"
    Q       = @Match "Q"
    s       = @Match "s"
    S       = @Match "S"
    v       = @Match "v"
    w       = @Match "w"
    W       = @Match "W"
    y       = @Match "y"
    quote   = @Match "'"
    dquote  = @Match "\""

-- Skip image data
def SkipImageData =
  block
    Many $[!'E']
    $['E']
    @$['I'] <| SkipImageData

