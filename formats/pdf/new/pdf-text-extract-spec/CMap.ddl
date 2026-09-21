import Daedalus
import PdfValue
import PdfDecl
import StandardEncodings
import Debug

--------------------------------------------------------------------------------
-- Character Maps
--
-- Character maps describe how character codes in PDF text strings map to
-- Unicode sequences. This module parses ToUnicode CMaps for text extraction.


def ToUnicodeCMap (v : Value) =
  case v of
    ref r ->
      case ResolveDeclRef r of
        value v  -> {| named = v is name |}
        stream s -> {| cmap = ToUnicodeCMapDef s |}
    name x -> {| named = x |}

def ToUnicodeCMapDef (s : Stream) =
  block
    SetStream (s.body is ok)
    CMapPrologue
    $$ = cmap (CMapEntries cmapBuilder)
    KW "endcmap"

-- The CMap data is a PostScript program, but text extraction only needs the
-- portion between begincmap and endcmap. Skip wrapper declarations, comments,
-- and resource setup rather than requiring one particular prologue.
def CMapPrologue =
  KW "begincmap"
  <| block
       UInt8
       CMapPrologue

def CMapEntries acc =
  case LookAhead UInt8 of
    '/' ->
      block
        -- Parse and ignore CMap metadata entries.
        CMapKeyVal
        CMapEntries acc
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ->
      CMapEntries (CMapOperator acc)
    _ -> acc

def CMapOperator (acc : cmapBuilder) =
  block
    let size = Token Natural as? uint 64
    size <= 100 is true
    Match "begin"
    let op = Token (Many $['a' .. 'z'])
    $$ =
      case op of
        "bfrange"         -> BFRangeMapping acc size
        "bfchar"          -> BFCharMapping acc size
        "codespacerange"  -> CodespaceRangeMapping acc size
        _ -> Fail (concat [ "Unsupported CMap operator `begin", op, "`" ])
    Match "end"
    Token (Match op)


-- Parse the requested number of bfrange entries, expanding each source-code
-- range into individual, width-preserving entries in the character map.
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
      let text = build first.output
      map (amount in rangeUp count)
        map (i, codePoint in text)
          if i + 1 == length text
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
            build destination.output
      Token $[']']


-- Parse the requested number of bfchar entries, adding each source code and
-- its Unicode destination sequence to the character map.
def BFCharMapping acc count =
  if count > 0 then
    block
      let source = Token SourceCode
      let destination = Token { $['<']; $$ = UTF16BE 512; $['>'] }
      let key = source.width <# source.value
      BFCharMapping
        (insertChar key (build destination.output) acc)
        (count - 1)
  else
    acc

-- Parse codespace ranges directly into the builder-backed CMap accumulator.
def CodespaceRangeMapping acc count =
  if count > 0 then
    CodespaceRangeMapping
      (addCodespaceRange CodespaceRangeEntry acc)
      (count - 1)
  else
    acc

def CodespaceRangeEntry =
  block
    start = Token SourceCode
    end   = Token SourceCode
    start.width == end.width is true
    start.value <= end.value is true

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


def CMapKeyVal =
  block
    Name
    @CMapPSDict <| @Value
    Optional (KW "def")

-- ISO 32000-2:2017, 9.7.5.4 uses this PostScript dictionary form for
-- CIDSystemInfo. Other metadata values use the ordinary PDF value syntax.
def CMapPSDict =
  block
    Token Natural
    KW "dict"
    Optional (KW "dup")
    KW "begin"
    Many CMapKeyVal
    KW "end"

-- Parsing hex digits

def HexD =
  First
    $['0' .. '9'] - '0'
    10 + $['a' .. 'f'] - 'a'
    10 + $['A' .. 'F'] - 'A'

def HexByte = 16 * HexD + HexD

def UTF16BE (maxBytes : uint 64) =
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



-- Helpers for working with cmaps

def cmapBuilder =
  block
    charMap = empty : [ uint 64 -> [uint 32] ]
    ranges  = builder : builder CodespaceRangeEntry

def cmap (acc : cmapBuilder) =
  block
    charMap = acc.charMap
    ranges  = build acc.ranges

def insertChar x y (acc : cmapBuilder) : cmapBuilder =
  block
    charMap = insert x y acc.charMap
    ranges  = acc.ranges

def addCodespaceRange x (acc : cmapBuilder) : cmapBuilder =
  block
    charMap = acc.charMap
    ranges  = emit acc.ranges x



