import Daedalus
import PdfValue
import PdfDecl
import CodespaceRange

--------------------------------------------------------------------------------
-- Character Maps

-- Parse a unicode character map out of a PDF value
def UnicodeCMap (v : Value) =
  case v of
    ref r ->
      case ResolveDeclRef r of
        value v  -> {| named = v is name |}
        stream s -> {| cmap = CMap nothing s |}
    name x -> {| named = x |}


-- Parse a character map, with the given parent.
def CMap (parent: maybe cmap) (s: Stream) =
  block
    SetStream (s.body is ok)
    CMapPrologue
    let initial =
      case parent of
        nothing ->
          block
            codeSpace    = codespaceTrie
            codeMapping  = cmapMappings
        just p ->
          block
            codeSpace = p.codeSpace
            codeMapping =
              block
                codeValue = empty
                parent    = just p.codeMapping
    $$ = CMapEntries initial
    KW "endcmap"



-- A character map contains information on:
-- (a) how to convert PDF bytes into character codes, and
-- (b) how to map charcter codes to unicode. 
-- Unicode is encoded as UTF16BE.
def cmap =
  block
    codeMapping = cmapMappings    -- How to map character codes to unicode.
    codeSpace   = codespaceTrie   -- How to decode bytes into character codes.

-- An empty table for mapping character codes to to unicode.
def cmapMappings =
  block
    codeValue = empty : [SourceCode -> cmapMapping] -- Local entries
    parent    = nothing : maybe cmapMappings        -- Inherited entries

-- An entry mapping a character code to UTF
def cmapMapping =
  union
    Single:     [uint 16]               -- One source code mapped to one unicode sequence.
    Range:      (uint 32, [uint 16])    -- A source range mapped by incrementing the final destination code unit.
    ArrayRange: (uint 32, [[uint 16]])  -- A source range with one explicit unicode sequence per source code.

-- The CMap data is a PostScript program, but text extraction only needs the
-- portion between begincmap and endcmap. Skip wrapper declarations, comments,
-- and resource setup rather than requiring one particular prologue.
def CMapPrologue =
  KW "begincmap"
  <| block
       UInt8
       CMapPrologue



--------------------------------------------------------------------------------
-- Parsing CMap Entries
--------------------------------------------------------------------------------

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

def cmapEntry =
  union
    Mapping:   cmapMapping
    Codespace: SourceCode

-- An entry for a range of input codes
def BFRangeEntry (start : SourceCode) : cmapEntry =
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
def BFCharEntry (source : SourceCode) : cmapEntry =
  Between "<" ">" {| Mapping = {| Single = UTF16BE |} |}

-- Codespace ranges determine how many input bytes form each source code.
def CodespaceRangeEntry (start : SourceCode) : cmapEntry =
  block
    let end = Token SourceCode
    start.width == end.width is true
    start.value <= end.value is true
    {| Codespace = end |}

-- Destination strings are retained as arrays of raw UTF-16BE code units.
-- Unicode scalar validation is deferred to the Rust emitter.
def UTF16BE = Many (..256) (HexByte # HexByte)


def mappingEnd (source : SourceCode) (mapping : cmapMapping) : uint 32 =
  case mapping of
    Single _       -> source.value
    Range range    -> range.0
    ArrayRange range -> range.0

def mappingDisjoint
  (key : SourceCode)
  (mapping : cmapMapping)
  (existing : (SourceCode, cmapMapping)) : bool =
  key.width != existing.0.width ||
  mappingEnd key mapping < existing.0.value ||
  mappingEnd existing.0 existing.1 < key.value


-- Add a parsed entry to the existing map
def InsertEntry (source : SourceCode) (entry : cmapEntry) (acc : cmap) : cmap =
  case entry of
    Mapping mapping ->
      block
        -- Check for an earlier mapping that extends into the new range.
        case lookupLE source acc.codeMapping.codeValue of
          nothing  -> Accept
          just old -> mappingDisjoint source mapping old is true

        let endSource =
          { width = source.width, value = mappingEnd source mapping }
        -- Check for an existing mapping that starts inside the new range.
        case lookupLE endSource acc.codeMapping.codeValue of
          nothing  -> Accept
          just old -> mappingDisjoint source mapping old is true

        codeSpace = acc.codeSpace
        codeMapping =
          block
            codeValue = insert source mapping acc.codeMapping.codeValue
            parent    = acc.codeMapping.parent

    Codespace end ->
      block
        codeSpace   = InsertCodespace source end acc.codeSpace
        codeMapping = acc.codeMapping




--------------------------------------------------------------------------------
-- Parsing CMap Metadata (ignored)
--------------------------------------------------------------------------------

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

