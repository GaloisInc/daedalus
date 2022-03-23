import PdfValue
import PdfDecl

--------------------------------------------------------------------------------
-- Section 7.7.2, Table 29
def PdfCatalog (r : Ref) =
  block
    let d = ResolveValRef r is dict
    pageTree = PdfPageTreeRoot (LookupRef "Pages" d)
    -- other fields omitted


--------------------------------------------------------------------------------
-- Page Tree; Section 7.7.3

def PdfPageTreeRoot (r : Ref) = PdfPageTree nothing empty r

def PdfPageTree (p : maybe Ref) (pResources : Dict) (r : Ref) =
  block
    let node = ResolveValRef r is dict
    PdfCheckParent p node
    let resources = case Optional (Lookup "Resources" node) of
                      just v  -> ResolveVal v is dict
                      nothing -> pResources
    let type = LookupResolve "Type" node is name
    if type == "Pages"
       then {| Node = map (child in (LookupResolve "Kids" node is array))
                          (PdfPageTree (just r) resources (child is ref))
            |}

       else if type == "Page"
              then {| Leaf = PdfPage resources node |}
              else Fail "Unexpected `Type` in page tree"

def PdfCheckParent (p : maybe Ref) (d : Dict) =
  case Optional (Lookup "Parent" d) of
    nothing -> p is nothing
    just v  -> p == just (v is ref) is true
  <| Fail "Malformed node parent"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Page. Section 7.7.3.3, Table 31

def PdfPage (resources : Dict) (pageNode : Dict) =
  case Optional (Lookup "Contents" pageNode) of
    nothing -> {| EmptyPage = Accept |}
    just vr -> {| ContentStreams = PdfPageContent resources vr |}

def PdfPageContent (resources : Dict) (vr : Value) =
  block
    resources = resources
    SetStream
      case vr of
        ref r    -> ContentStreamBytes r

        -- inefficient!
        array xs ->
          block
            let chunks =
                  map (x in xs)
                    block
                    let bs = bytesOfStream (ContentStreamBytes (x is ref))
                    concat [bs, " "] -- yikes.

            arrayStream (concat chunks)

    data = block
             ManyWS
             Many ContentStreamEntry

    UNPARSED = if ?strict then { END; [] } else Many UInt8

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
    @(Match "EI") <| SkipImageData

--------------------------------------------------------------------------------
-- Simple Text Extraction


def TextInCatalog (c : PdfCatalog) = reverse nil (TextInPageTree nil c.pageTree)

def TextInPageTree acc (t : PdfPageTree) =
  case t of
    Node kids -> for (s = acc; x in kids) (TextInPageTree s x)
    Leaf p -> TextInPage acc p

def TextInPage acc (p : PdfPage) =
  case p of
    EmptyPage -> acc
    ContentStreams content -> TextInPageContnet acc content

def TextInPageContnet acc (p : PdfPageContent) =
  block
    let ?instrs = p.data
    FindTextOnPage acc 0

def GetOperand i = (Index ?instrs i : ContentStreamEntry) is value

def FindTextOnPage acc i =
  case Optional (Index ?instrs i) of
    nothing -> acc
    just instr ->
      case instr of

        operator op ->
          case op of

            Tj, quote, dquote ->
              FindTextOnPage
                (push (GetOperand (i - 1) is string) acc)
                (i+1)

            TJ ->
              for (s = acc; x in GetOperand (i - 1) is array)
                case x of
                  string v -> push v s
                  _        -> s

            _  -> FindTextOnPage acc (i+1)

        _  -> FindTextOnPage acc (i+1)



--------------------------------------------------------------------------------
-- Just used for the type (lists/builders should be a standard type)
def List P =
  First
    cons = Node P
    nil  = Accept

def Node P =
  block
    head = P
    tail = List P

def nil : List = {| nil |}
def push x xs : List = {| cons = { head = x, tail = xs } |}

def reverse acc (xs : List) =
  case xs of
    nil    -> acc
    cons n -> reverse (push n.head acc) n.tail




