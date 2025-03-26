import Daedalus
import Debug
import utf8

-- Parse with a numeric Parser and ensure that it is
-- within a specfied range
def NumericParserWithRange P min max =
    block
        let v = P
        GuardMsg ( v >= min && v <= max ) "Value not in range"
        ^ v

-- Parse a byte value and turn it in to a boolean
-- Note that the parser is strict in what it considers 'true'
def BooleanFlag =
    block
        let v = UInt8
        ^ (toBoolean v)

-- Convert from a numeric value to boolean
-- It is strict in what it considers 'true'
def toBoolean v =
    if v == 0
        then false
        else true
        
-- Read the next 4 bytes
def FourByteString = Many 4 UInt8

-- Find the index of an entry in an array. Return -1 if not found
def indexOf (entry : ?a) (arr : [?a]): int =
    block
        for (val = -1 : int;  i, v in arr)
            if (val == -1 && v == entry) then (i as int) else val

-- Parser a UTF8 encoded null-terminated string
def UTF8NTString =
    block
        -- First find the null block
        let start = Offset
        let nullOffset = LookAhead OffsetOfNull
        -- Take the relevant values as bytes
        -- NOTE: We skip the NULL character (which may or may not be what we want)
        -- NOTE: We could take them as UTF8 bytes too
        --  by using the stream
        let count = nullOffset - start - 1
        Many count UInt8

-- Find the offset of the null value
def OffsetOfNull =
    block
        let done =
            many (done = false)
                block
                    done is false
                    UTF8 == 0
        case done of
            true  -> Offset
            false -> Fail "Malformed String"


-----------------------------------------------------------------------------------
-- Box Parsing Helpers
-----------------------------------------------------------------------------------

def BoxHeader =
    block
        let lboxValue = BEUInt32
        type          = FourByteString   -- Technically this should be BEUInt32, this is easier for comparisons
        boxLength     = BoxLength lboxValue

def Box ContentParser =
    block
        header  = BoxHeader
        content = BoxContent header.boxLength ContentParser

def BoxOfType type ContentParser =
    block
        -- Parse header and check for type
        header  = BoxHeader
        Guard ( header.type == type )
        -- At this point, we know what we are parsing, so any
        -- failure can and should be treated as fatal
        commit
        -- Parse the contents
        content = BoxContent header.boxLength ContentParser
  
def BoxLength (lboxValue : uint 32) =
    block
        case lboxValue of
            0x0 -> {| untilEndOfFile |}
            0x1 -> 
                block
                    let v = BEUInt64
                    {| fixedSizeLarge = v  |}
            _   -> {| fixedSizeSmall = lboxValue |}

def contentLength (boxLength: BoxLength) =
    block
        case boxLength of
            fixedSizeLarge sz -> (sz - 16) as uint 64  -- sizes of lbox, tbox and xlbox to be ignored
            fixedSizeSmall sz -> (sz - 8)  as uint 64   -- sizes of lbox, tbox to be ignored

def BoxContent (boxLength: BoxLength) ContentParser =
    block
        case boxLength of
            untilEndOfFile -> Only ContentParser
            _              -> Chunk (contentLength boxLength) (Only ContentParser)

-----------------------------------------------------------------------------------
-- Marker Parsing Helpers
-----------------------------------------------------------------------------------

-- Specific marker
def Marker x = { $[0xFF] ; $[x] }

-- Parse *any* marker
def AnyMarker =
  block
    $[0xFF]
    UInt8

-- Helper to parse the parameters after a marker
def Parameters Parser =
    block
        let size = BEUInt16 as uint 64
        size >= 2 is true
        let len = size - 2
        Chunk len Parser

-- Parse a marker segment
def MarkerSegment Parser =
    block
        marker     = AnyMarker
        parameters = Parameters Parser

-- Parse a marker segment and ensure that it is of a specific
-- type before parsing parameters
def MarkerSegmentOfType type Parser =
    block
        marker     = Marker type
        commit
        parameters = Parameters Parser

-- Parse as many entities as you can until we hit the specified marker
-- This works correctly only if the specified parser always stops before
-- markers (for example if the parser was a marker segment parser of some sort)
def UntilMarker markerID Parser = Many (UnlessMarker markerID Parser)

-- Parse unless you have hit a specified marker
def UnlessMarker markerID Parser =
    block
        let found = LookAhead AnyMarker
        --Trace (dbgShowHexNumber found)
        let x = (markerID == found)
        --Trace (if (x) then "T" else "F")
        markerID == found is false 
        Parser
