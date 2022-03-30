import PdfValue
import PdfDecl
import Debug

--------------------------------------------------------------------------------
-- Section 7.7.2, Table 29
def PdfCatalog (strict : bool) (r : Ref) =
  block
    let ?strict = strict
    let d = ResolveValRef r is dict
    pageTree = PdfPageTreeRoot (LookupRef "Pages" d)
    -- other fields omitted


--------------------------------------------------------------------------------
-- Page Tree; Section 7.7.3

def PdfPageTreeRoot (r : Ref) = PdfPageTree nothing noResources r

def PdfPageTree (p : maybe Ref) (parentResources : Resources) (r : Ref) =
  block
    let node = ResolveValRef r is dict
    PdfCheckParent p node

    {- Since resources are shared, we want to process them once at the
    node and pass the processed resources down the tree, otherwise
    we are going to reprocess them again for each leaf which is a lot
    of repeated work in large doucuments. -}
    let resources = case Optional (Lookup "Resources" node) of
                      just v  -> Resources v <| parentResources -- if `GetFonts` fails in `Resources` then assign the parentResources
                      nothing -> parentResources
    let type = LookupResolve "Type" node is name
    if type == "Pages"
       then {| Node = map (child in (LookupResolve "Kids" node is array))
                          (PdfPageTree (just r) resources (child is ref))
            |}

       else if type == "Page"
              then {| Leaf = PdfPage resources node |}
              else Fail "Unexpected `Type` in page tree"

def noResources : Resources =
  block
    fonts = empty

def Resources (v : Value) =
  block
    fonts = GetFonts (ResolveVal v is dict)
    -- others resources omitted
    -- we need the fonts because they determine the character encoding to use

def PdfCheckParent (p : maybe Ref) (d : Dict) =
  (case Optional (Lookup "Parent" d) of
    nothing -> p is nothing
    just v  -> p == just (v is ref) is true
  )
   <| Fail "Malformed node parent"

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Page. Section 7.7.3.3, Table 31

def PdfPage (resources : Resources) (pageNode : Dict) =
  case Optional (Lookup "Contents" pageNode) of
    nothing -> {| EmptyPage = Accept |}
    just vr -> {| ContentStreams = PdfPageContent resources vr |}

def PdfPageContent (resources : Resources) (vr : Value) =
  block
    resources = resources

    SetStream
      case vr of
        ref r ->
          case ResolveDeclRef r of
            value v  -> StreamFromArray (v is array)
            stream s -> ( s.body is ok
                         <|
                          { @x = s.body is undecoded
                          ; Trace "WARNING: undecoded stream"
                          ; ^ x }
                        )

        array xs -> StreamFromArray xs

    data = block
             ManyWS
             Many ContentStreamEntry

    UNPARSED = if ?strict then { END; [] } else Many UInt8


        -- inefficient!
def StreamFromArray (xs : [Value]) =
  block
    let chunks = map (x in xs)
                    block
                    let bs = bytesOfStream (ContentStreamBytes (x is ref))
                    concat [bs, " "] -- yikes.

    arrayStream (concat chunks)



def ContentStreamBytes (r : Ref) : stream = block
  let bdy = (ResolveStreamRef r).body
  ( bdy is ok
    <|
      { @x = bdy is undecoded
      ; Trace "WARNING: undecoded ContentStreamBytes"
      ; ^ x
      }
    )


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

--------------------------------------------------------------------------------

-- XXX: It'd be nice to cache preocessed fonts by reference so if we encounter
-- the same reference we don't parse it over and over again.
-- (this might be a generally useful thing to have)
def GetFonts (r : Dict) : [ [uint 8] -> Font ] =
  case Optional (Lookup "Font" r) of
    nothing -> empty
    just v  ->
      map (v in (ResolveVal v is dict)) (Font v)

def Font (v : Value) =
  block
    let dict = ResolveVal v is dict
    subType = ResolveVal (Lookup "Subtype" dict) is name
    encoding = Optional (Lookup "Encoding" dict)
    toUnicode = case Optional (Lookup "ToUnicode" dict) of
                  nothing -> nothing
                  just v  -> just (ToUnicodeCMap v)

--------------------------------------------------------------------------------
-- Character Maps

def ToUnicodeCMap (v : Value) =
  case v of
    ref r ->
      case ResolveDeclRef r of
        value v  -> {| named = v is name |}
        stream s -> {| cmap = ToUnicodeCMapDef s |}
    name x -> {| named = x |}

def CMapScope begin end P =
  block
    KW begin
    $$ = P
    KW end

-- XXX: .. in patterns
def Hex64 (acc : uint 64) =
  block
    let b = UInt8
    case b of
      '0', '1','2','3','4','5','6','7','8','9' ->
        Hex64 (16 * acc + ((b - '0') as ?auto))

      'a', 'b', 'c', 'd', 'e', 'f' ->
        Hex64 (16 * acc + ((b - 'a') as ?auto))

      'A', 'B', 'C', 'D', 'E', 'F' ->
        Hex64 (16 * acc + ((b - 'A') as ?auto))

      '>' -> acc

def Hex = block $['<']; Hex64 0



def ToUnicodeCMapDef (s : Stream) =
  block
    SetStream (s.body is ok)
    ManyWS
    Name == "CIDInit" is true
    Name == "ProcSet" is true
    KW "findresource"
    KW "begin"
    -- XXX: don't quite understand the dict/dup format thing
    let size = Token Natural
    KW "dict"
    -- XXX: don't quite understand the dict/dup format thing
    KW "begin"
    KW "begincmap"
    $$ = Many CMapEntry
    KW "endcmap"

    -- XXX: ignore rest


def CMapEntry =
  block
    First
      keyVal   = CMapKeyVal
      operator = CMapOperator


def CMapKeyVal =
  block
    key   = Name
    value = CMapValue
    Optional (KW "def")

def CMapValue =
  First
    dict    = CMapDict
    string  = String
    string  = HexString
    name    = Name
    number  = Number

def CMapDict =
  block
    let ents = Between "<<" ">>" (Many CMapKeyVal)
    for (d = empty; e in ents) (insert e.key e.value d)

def CMapOperator =
  block
    let size = Token Natural as? uint 64
    size <= 100 is true
    KW "begin"
    op = Token (Many $['a' .. 'z'])
    let w = if op == "codespacerange" then 2 else
            if op == "bfrange"        then 3 else
            if op == "bfchar"         then 2 else
            Fail "Unknown op"
    args = Many size (Many w (Token Hex))
    KW "end"
    Token (Match op)





--------------------------------------------------------------------------------
-- Simple Text Extraction

def TextInCatalog (c : PdfCatalog) =
  reverse nil (TextInPageTree nil c.pageTree)

def TextInPageTree acc (t : PdfPageTree) =
  case t of
    Node kids -> for (s = acc; x in kids) (TextInPageTree s x)
    Leaf p -> TextInPage acc p

def TextInPage acc (p : PdfPage) =
  case p of
    EmptyPage -> acc
    ContentStreams content ->
      block
        let ?resources = content.resources
        TextInPageContnet acc content

def TextInPageContnet acc (p : PdfPageContent) =
  block
    let ?instrs = p.data
    FindTextOnPage acc 0

def Dump (xs : List) =
  case xs of
    cons x ->
      block
        case x.head : TextItem of
          word w -> Trace w
          font   -> Trace "<FONT>"
        Dump x.tail
    nil -> Accept

def GetOperand i = (Index ?instrs i : ContentStreamEntry) is value

def FindTextOnPage acc i =
  case Optional (Index ?instrs i) of
    nothing -> acc
    just instr ->
      case instr of

        operator op ->
          case op of

            Tj, quote, dquote ->
              block
                let word = {| word = GetOperand (i - 1) is string |} : TextItem
                FindTextOnPage (push word acc) (i+1)

            TJ ->
              block
                let acc1 = for (s = acc; x in GetOperand (i - 1) is array)
                             case x of
                               string v -> push {| word = v |} s
                               _        -> s
                FindTextOnPage acc1 (i+1)

            Tf ->
              block
                let fontName = GetOperand (i - 2) is name
                let font     = Lookup fontName ?resources.fonts
                FindTextOnPage (push {| font = font |} acc) (i+1)

            _  -> FindTextOnPage acc (i+1)

        _  -> FindTextOnPage acc (i+1)

-- type only
def TextItem =
     {| word = [] : [uint 8] |}
  <| {| font = Fail "unused" : Font |}

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

def DumpStream (s : Stream) =
  block
    SetStream (s.body is ok)
    Many UInt8


--------------------------------------------------------------------------------
-- Hacky "validation" we look for JavaScript or URI actions

-- Return `true` if this declaration is "safe"
def CheckRef (r : Ref) : bool =
  case ResolveDeclRef r of
    stream  -> true
    value v ->
      First
        block
          FindUnsafe v
          false
        true


def FindUnsafe (v : Value) =
  case v of
    dict d ->
      First
        block
          let act = ResolveVal (Lookup "S" d) is name
          (act == "JavaScript" || act == "URI") is true
        @Lookup "JS" d

    array xs ->
      block
        let ?vals = xs
        AnyUnsafe 0

    _        -> Fail "not unsafe"


def AnyUnsafe i =
  block
    let v = Index ?vals i
    FindUnsafe v <| AnyUnsafe (i+1)


