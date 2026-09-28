import Daedalus
import PdfValue
import PdfDecl
import CodespaceRange

--------------------------------------------------------------------------------
-- Character Maps

def CMap =
  block
    codespace = codespaceTrie -- see CodespaceRange
    rangeMap  = empty : [SourceCode -> CMapMapping]

def CMapMapping =
  union
    -- One source code mapped to one UTF-16BE sequence.
    Single:     [uint 16]

    -- A source range mapped by incrementing the final destination code unit.
    Range:      (uint 32, [uint 16])

    -- A source range with one explicit UTF-16BE sequence per source code.
    ArrayRange: (uint 32, [[uint 16]])

def CMapEntry =
  union
    Mapping: CMapMapping
    Codespace: SourceCode

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
    $$ = CMapEntries CMap
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
        CMapKeyVal
        CMapEntries acc
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ->
      CMapEntries (CMapOperator acc)
    _ -> acc

def CMapOperator (acc : CMap) =
  block
    let size = Token Natural as? uint 64
    size <= 100 is true
    Match "begin"
    let op = Token (Many $['a' .. 'z'])
    $$ =
      case op of
        "bfrange"         -> CMapMappings BFRangeEntry acc size
        "bfchar"          -> CMapMappings BFCharEntry acc size
        "codespacerange"  -> CMapMappings CodespaceRangeEntry acc size
        _ -> Fail (concat [ "Unsupported CMap operator `begin", op, "`" ])
    Match "end"
    Token (Match op)


-- Parse a sequence of entries that share the same source-key/accumulator
-- structure, while leaving the entry-specific syntax to P.
def CMapMappings P acc count =
  if count > 0 then
    block
      let source = Token SourceCode
      let entry = P source
      CMapMappings P (InsertEntry source entry acc) (count - 1)
  else
    acc


-- An entry for a range of input codes
def BFRangeEntry (start : SourceCode) : CMapEntry =
  block
    let end = Token SourceCode
    start.width == end.width is true
    start.value <= end.value is true
    let rangeCount =
      (end.value as uint 64) - (start.value as uint 64) + 1

    Token
      First
        block
          let units = Between "<" ">" UTF16BE
          length units > 0 is true
          let last = Index units (length units - 1)
          let lastByte = last as! uint 8
          (lastByte as uint 64) + rangeCount - 1 <= 255 is true
          {| Mapping = {| Range = (end.value, units) |} |}

        block
          let destinations =
            Between "[" "]"
              (Many rangeCount
                (Between "<" ">" UTF16BE))
          {| Mapping = {| ArrayRange = (end.value, destinations) |} |}


-- An entry for a single code
def BFCharEntry (source : SourceCode) : CMapEntry =
  Between "<" ">" {| Mapping = {| Single = UTF16BE |} |}

-- Codespace ranges determine how many input bytes form each source code.
def CodespaceRangeEntry (start : SourceCode) : CMapEntry =
  block
    let end = Token SourceCode
    start.width == end.width is true
    start.value <= end.value is true
    {| Codespace = end |}

def CMapKeyVal =
  block
    Name
    @CMapPSDict <| @Value
    Optional (KW "def")

-- ISO 32000-2:2017, 9.7.5.4 uses a PostScript dictionary for CIDSystemInfo.
-- Some generated CMaps use `<< >>` delimiters while still terminating each
-- entry with `def`, so this parser accepts both common representations.
def CMapPSDict =
  First
    block
      KW "<<"
      Many CMapKeyVal
      KW ">>"

    block
      Token Natural
      KW "dict"
      Optional (KW "dup")
      KW "begin"
      Many CMapKeyVal
      KW "end"


-- Destination strings are retained as arrays of raw UTF-16BE code units.
-- Unicode scalar validation is deferred to the Rust emitter.
def UTF16BE = Many (..256) (HexByte # HexByte)


def InsertEntry
  (source : SourceCode)
  (entry : CMapEntry)
  (acc : CMap) : CMap =
  case entry of
    Mapping mapping ->
      block
        rangeMap = insert source mapping acc.rangeMap
        codespace = acc.codespace

    Codespace end ->
      block
        rangeMap = acc.rangeMap
        codespace = InsertCodespace source end acc.codespace
