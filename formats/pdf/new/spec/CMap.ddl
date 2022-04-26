import PdfValue
import PdfDecl
import StandardEncodings
import Debug

--------------------------------------------------------------------------------
-- Character Maps
--
-- NOTE: I don't fully understand the formats for these, and they don't
-- appear to work quite as specified here.


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


