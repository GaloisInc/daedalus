import Daedalus
import Debug

-- Parse with a numeric Parser and ensure that it is
-- within a specfied range
def NumericParserWithRange P min max =
    block
        let v = P
        Guard ( v >= min && v <= max )
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
        header  = BoxHeader
        Guard ( header.type == type )
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
            untilEndOfFile -> { $$ = ContentParser ; END }
            _              -> Chunk (contentLength boxLength) ContentParser

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

def Parameters Parser =
    block
        let size = BEUInt16 as uint 64
        size >= 2 is true
        let len = size - 2
        Chunk len Parser

def MarkerSegment Parser =
    block
        marker     = AnyMarker
        parameters = Parameters Parser

def MarkerSegmentOfType type Parser =
    block
        marker     = Marker type
        parameters = Parameters Parser


def UntilMarker markerID Parser = Many (UnlessMarker markerID Parser)

def UnlessMarker markerID Parser =
    block
        let found = LookAhead AnyMarker
        --Trace (dbgShowHexNumber found)
        let x = (markerID == found)
        --Trace (if (x) then "T" else "F")
        markerID == found is false 
        Parser

def UnlessOneOfTwoMarkers markerID1 markerID2 Parser =
    block
        let found = LookAhead AnyMarker 
        --Trace (dbgShowHexNumber found)
        let x = (markerID1 == found || markerID2 == found)
        --Trace (if (x) then "T" else "F")
        (markerID1 == found || markerID2 == found) is false ;
        Parser
