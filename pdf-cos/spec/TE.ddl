import PdfValue
import PdfDecl
import StandardEncodings
import Debug

--------------------------------------------------------------------------------
-- Section 7.7.2, Table 29
def PdfCatalog (strict : bool) (enc : maybe StdEncodings) (r : Ref) =
  block
    let ?strict = strict
    let ?doText = enc
    let d       = ResolveValRef r is dict
    pageTree = PdfPageTreeRoot (LookupRef "Pages" d)
    stdEncodings = case enc of
                     nothing -> noStdEncodings
                     just e  -> e
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
                      just v  -> Resources v
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
  case Optional (Lookup "Parent" d) of
    nothing -> p is nothing
    just v  -> p == just (v is ref) is true
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
            stream s -> s.body is ok

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

--------------------------------------------------------------------------------

-- XXX: It'd be nice to cache preocessed fonts by reference so if we encounter
-- the same reference we don't parse it over and over again.
-- (this might be a generally useful thing to have)
def GetFonts (r : Dict) : [ [uint 8] -> Font ] =
  case ?doText of
    nothing -> empty
    just enc ->
      block
        let ?stdEncodings = enc
        case Optional (Lookup "Font" r) of
          nothing -> empty
          just v  ->
            map (v in (ResolveVal v is dict)) (Font v)

def Font (v : Value) =
  block
    let dict = ResolveVal v is dict
    subType   = ResolveVal (Lookup "Subtype" dict) is name
    encoding  = GetEncoding dict
    toUnicode = case Optional (Lookup "ToUnicode" dict) of
                  nothing -> nothing
                  just v  -> just (ToUnicodeCMap v)

def namedEncoding (encName : [uint 8]) =
  if encName == "WinAnsiEncoding"  then just ?stdEncodings.win else
  if encName == "MacRomanEncoding" then just ?stdEncodings.mac else
  if encName == "PDFDocEncoding"   then just ?stdEncodings.pdf else
  if encName == "StandardEncoding" then just ?stdEncodings.std else
  nothing


def GetEncoding (d : Dict) : maybe [uint 8 -> [uint 16]] =
  case lookup "Encoding" d of
    nothing -> nothing
    just v ->
     block
      case ResolveVal v of
        name encName -> namedEncoding encName
        dict encD ->
          block
            let base = case lookup "BaseEncoding" encD of
                         just ev -> namedEncoding (ResolveVal ev is name)
                         nothing -> nothing
            case lookup "Differences" encD of
              nothing -> base
              just d  -> just (EncodingDifferences base (ResolveVal d is array))

        _ -> nothing


def EncodingDifferences base ds : [ uint 8 -> [uint 16] ]=
  block
    let start = case base of
                  nothing -> ?stdEncodings.std
                  just e  -> e
    let s = for (s = { enc = start, code = 0 }; x in ds)
             case ResolveVal x of
               number n -> { enc = s.enc, code = NumberAsNat n as? uint 8 }
               name x ->
                 { enc  = case lookup x ?stdEncodings.uni of
                            just u  -> insert s.code u s.enc
                            nothing ->
                              block
                                -- Trace (concat ["Missing: ", x])
                                insert s.code ['?' as ?auto] s.enc
                 , code = s.code + 1
                 }

    s.enc



--------------------------------------------------------------------------------
-- Character Maps

def ToUnicodeCMap (v : Value) =
  case v of
    ref r ->
      case ResolveDeclRef r of
        value v  -> {| named = v is name |}
        stream s -> {| cmap = ToUnicodeCMapDef s |}
    name x -> {| named = x |}

-- XXX: .. in patterns
def HexNum acc =
  block
    let b = UInt8
    case b of
      '0', '1','2','3','4','5','6','7','8','9' ->
        HexNum (16 * acc + ((b - '0') as ?auto))

      'a', 'b', 'c', 'd', 'e', 'f' ->
        HexNum (16 * acc + (10 + (b - 'a') as ?auto))

      'A', 'B', 'C', 'D', 'E', 'F' ->
        HexNum (16 * acc + (10 + (b - 'A') as ?auto))

      '>' -> acc

def HexD =
  First
    $['0' .. '9'] - '0'
    10 + $['a' .. 'f'] - 'a'
    10 + $['A' .. 'F'] - 'A'

def HexByte = 16 * HexD + HexD

def Hex : uint 32 = block $['<']; HexNum 0

def HexBytes (lo : uint 64) (hi : uint 64) =
  block
    Token $['<']
    $$ = Many (1 .. 4) HexByte
    Token $['>']


def ToUnicodeCMapDef (s : Stream) =
  block
    SetStream (s.body is ok)
    -- Trace (bytesOfStream (s.body is ok))
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
    $$ = CMapEntries cmap
    KW "endcmap"
    -- XXX: ignore rest


def cmap =
  block
    charMap = empty : [ uint 32 -> uint 32 ]
    ranges  = []    : [ CodespaceRangeEntry ]

def insertChar x y (acc : cmap) : cmap =
  block
    charMap = insert x y acc.charMap
    ranges  = acc.ranges

def addCodespaceRange xs (acc : cmap) : cmap =
  block
    charMap = acc.charMap
    ranges  = concat [ xs, acc.ranges ] -- yikes


def CMapEntries acc =
  case Optional CMapKeyVal of
    nothing ->
      case Optional (CMapOperator acc) of
        just acc1 -> CMapEntries acc1
        nothing   -> acc

    just _  -> CMapEntries acc   -- ignore metadata

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

def CMapOperator (acc : cmap) =
  block
    let size = Token Natural as? uint 64
    size <= 100 is true
    Match "begin"
    let op = Token (Many $['a' .. 'z'])
    $$ =      if op == "bfrange" then BFRangeMapping acc size
         else if op == "bfchar"  then BFCharMapping acc size
         else if op == "codespacerange" then
                  addCodespaceRange (Many size CodespaceRangeEntry) acc
         else Fail "Unsupported operator"
    Match "end"
    Token (Match op)

def CodespaceRangeEntry =
  block
    start = HexBytes 1 4
    end   = HexBytes 1 4

-- 00 00 -- FF FF

-- start    end
-- 00       50
-- 40 00    60 10

def BFCharMapping acc count =
  if count > 0 then
    block
      let key = Token Hex
      let val = Token Hex
      BFCharMapping (insertChar key val acc) (count - 1)
  else
    acc

def BFRangeMapping acc count =
  if count > 0 then
    block
      let start = Token Hex
      let end   = Token Hex
      let tgt   = Token Hex
      if end < start
        then BFRangeMapping acc (count - 1)
        else block
               let ?start  = start
               let ?target = tgt
               let ?count  = end - start + 1
               BFRangeMapping (insertRange 0 acc) (count - 1)
  else
    acc

def insertRange (i : uint 32) (acc : cmap) =
  if i < ?count
    then insertRange (i+1) (insertChar (?start + i) (?target + i) acc)
    else acc


def GetCharCode (cmap : cmap) : sint 32

def LookupCMap cmap =
  block
    let c = GetCharCode cmap
    EmitChar (Lookup (c as? uint 32) cmap.charMap <| ('?' as ?auto))

def LookupCMapLoop (w : uint 64) (prevIx : uint 32) =
  block
    let c = prevIx <# UInt8
    case Optional (Lookup c ?cmap) of
      just v  -> EmitChar v
      nothing -> if w < 16 then LookupCMapLoop (w + 8) c
                           else Fail "Unknown character code"





--------------------------------------------------------------------------------
-- Simple Text Extraction

def TextInCatalog (c : PdfCatalog) =
  block
    let ?stdEncodings = c.stdEncodings
    @(TextInPageTree nothing c.pageTree)

def TextInPageTree acc (t : PdfPageTree) =
  case t of
    Node kids -> for (s = acc; x in kids) (TextInPageTree s x)
    Leaf p    -> TextInPage acc p

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

def GetOperand i = (Index ?instrs i : ContentStreamEntry) is value

def FindTextOnPage acc i =
  case Optional (Index ?instrs i) of
    nothing -> acc
    just instr ->
      case instr of

        operator op ->
          case op of

            Tj ->
              block
                DecodeText acc (GetOperand (i - 1) is string)
                FindTextOnPage acc (i+1)


            quote, dquote ->
              block
                EmitChar ('\n' as ?auto)
                DecodeText acc (GetOperand (i - 1) is string)
                FindTextOnPage acc (i+1)

            Td, TD, T_star ->
              block
                EmitChar ('\n' as ?auto)
                FindTextOnPage acc (i+1)

            TJ ->
              block
                map (x in (GetOperand (i - 1) is array))  
                    case x of
                      string s -> DecodeText acc s
                      _        -> Accept -- EmitChar (' ' as ?auto)

                FindTextOnPage acc (i+1)

            Tf ->
              block
                let fontName = GetOperand (i - 2) is name
                let font     = Optional (Lookup fontName ?resources.fonts)
                FindTextOnPage font (i+1)

            _  -> FindTextOnPage acc (i+1)

        _  -> FindTextOnPage acc (i+1)

def DecodeText mbFont str =

  case mbFont of
    nothing -> Raw str

    just f ->
      if f.subType == "Type1" || f.subType == "Type3"
        then DecodeTextWithEncoding f str
        else
          case (f : Font).toUnicode of
            nothing -> Raw str
            just cmap ->
              case cmap of
                named x -> Raw str

                cmap c ->
                  block
                    let s = GetStream
                    SetStream (arrayStream str)
                    Many (LookupCMap c)
                    SetStream s


def DecodeTextWithEncoding (f : Font) str =
  block
    let enc = case f.encoding of
                nothing  -> ?stdEncodings.std
                just enc -> enc
    @map (x in str)
       case lookup x enc of
         just us -> @map (u in us) (EmitChar (u as ?auto))
         nothing -> EmitChar ('.' as ?auto)



def Raw (str : [uint 8]) = @map (x in str) (EmitChar (x as ?auto))

-- Emit a character
def EmitChar (c : uint 32) : {}


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


