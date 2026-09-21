import Daedalus
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

def SourceCode =
  block
    $['<']
    $$ =
      many (state = { width = (0 : uint 64), value = (0 : uint 32) })
        block
          state.width < 4 is true
          width = state.width + 1
          value = state.value <# HexByte
    $$.width > 0 is true
    $['>']

def UTF16BE (maxBytes : uint 64) =
  block
    let result =
      many
        (state =
          { bytes = (0 : uint 64)
          , lastByte = (0 : uint 8)
          , output = builder
          })
        block
          let hiByte = HexByte
          let loByte = HexByte
          let hi = hiByte # loByte
          if 0xD800 <= hi && hi <= 0xDBFF
            then
              block
                let surrogateHi = HexByte
                let surrogateLo = HexByte
                let lo = surrogateHi # surrogateLo
                bytes = state.bytes + 4
                bytes <= maxBytes is true
                (0xDC00 <= lo && lo <= 0xDFFF) is true
                let codePoint =
                  0x10000 +
                    ((hi as uint 32) - 0xD800) * 0x400 +
                    ((lo as uint 32) - 0xDC00)
                
                lastByte = surrogateLo
                output = emit state.output codePoint
            else
              block
                let bytes = state.bytes + 2
                bytes <= maxBytes is true
                (hi < 0xDC00 || hi > 0xDFFF) is true
                bytes = bytes
                lastByte = loByte
                output = emit state.output (hi as uint 32)
    bytes = result.bytes
    lastByte = result.lastByte
    text = build result.output

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
    charMap = empty : [ uint 64 -> [uint 32] ]
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
    start = Token SourceCode
    end   = Token SourceCode
    start.width == end.width is true
    start.value <= end.value is true

def BFCharMapping acc count =
  if count > 0 then
    block
      let source = Token SourceCode
      let destination = Token { $['<']; $$ = UTF16BE 512; $['>'] }
      let key = source.width <# source.value
      BFCharMapping (insertChar key destination.text acc) (count - 1)
  else
    acc

def BFRangeMapping acc count =
  if count > 0 then
    block
      let start = Token SourceCode
      let end   = Token SourceCode
      start.width == end.width is true
      if end.value < start.value
        then BFRangeMapping acc (count - 1)
        else
          block
            let rangeCount =
              (end.value as uint 64) - (start.value as uint 64) + 1
            let destinations = BFRangeDestinations rangeCount
            let result =
              for (result = acc; i, destination in destinations)
                block
                  let key = start.width <# (start.value + (i as! uint 32))
                  insertChar key destination result
            BFRangeMapping result (count - 1)
  else
    acc

-- ISO 32000-2:2017, 9.10.3, pp. 355-356 describes the two forms of
-- destination mappings permitted after `beginbfrange`.
def BFRangeDestinations (count : uint 64) =
  First
    -- In the starting-string form, increment only the final byte for each
    -- consecutive source code, without allowing that byte to exceed 255.
    block
      let first = Token { $['<']; $$ = UTF16BE 512; $['>'] }
      first.bytes > 0 is true
      (first.lastByte as uint 64) + count - 1 <= 255 is true
      map (amount in rangeUp count)
        map (i, codePoint in first.text)
          if i + 1 == length first.text
            then codePoint + (amount as! uint 32)
            else codePoint

    -- In the array form, there shall be exactly one destination string for
    -- each source code in the range.
    block
      Token $['[']
      $$ =
        Many count
          block
            let destination = Token { $['<']; $$ = UTF16BE 512; $['>'] }
            destination.text
      Token $[']']
