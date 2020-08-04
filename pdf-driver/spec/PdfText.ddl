--------------------------------------------------------------------------------
-- Futher validation of basic fields
--------------------------------------------------------------------------------

import ISOCodes
import PdfValue

--------------------------------------------------------------------------------
-- Section 7.8.2: content streams:

-- ContentStreamOp: the content stream operations. Table A.1:
def ContentStreamOp = Choose {
  closeFillStrokeNzWinding = @"b" ;
  fillStroke = @"B" ;
  closeFillStrokeEvenOdd = @"b*" ;
  fillStrokeEvenOdd = @"B*" ;
  beginMarkedContent = @"BDC" ;
  beginInline = @"Bl" ;
  beginMarkedContent = @"BMC" ;
  beginText = @"BT" ;
  appendCurvedThreePoints = @"c" ;
  concatMatrix = @"cm" ;
  setColorSpaceStroking = @"CS" ;
  setColorSpaceNonStroking = @"cs" ;
  setLineDash = @"d" ;
  setGlyphWidth = @"d0" ;
  setGlpyhWidthBoundingBox = @"d1" ;
  invokeXObj = @"Do" ;
  defMarkedContentPoint = @"DP" ;
  endInline = @"El" ;
  endMarkedContent = @"EMC" ;
  endTextObj = @"ET" ;
  fillPathNzWinding = @"f" ;
  fillPathNzWindingOld = @"F" ;
  fillPathEvenOdd = @"f*" ;
  setGrayStroking = @"G" ;
  setGrayNonStroking = @"g" ;
  setGraphicsStateParams = @"gs" ;
  closeSubpath = @"h" ;
  setFlat = @"i" ;
  beginInlineImageData = @"ID" ;
  setLineJoinStyle = @"j" ;
  setLineCapStyle = @"J" ;
  setCMYKStroking = @"K" ;
  setCMYKNonStroking = @"k" ;
  appendLine = @"l" ;
  beginNewSuppath = @"m" ;
  setMiterLimit = @"M" ;
  defineMarkedContent = @"MP" ;
  endPath = @"n" ;
  saveGraphicsState = @"q" ;
  restoreGraphicsState = @"Q" ;
  appendRect = @"re" ;
  setRGBStroking = @"RG" ;
  setRGBNonStroking = @"rg" ;
  setColorRenderingIntent = @"ri" ;
  closeStrokePath = @"s" ;
  stroke = @"S" ;
  setColorStroking = @"SC" ;
  setColorNonStroking = @"sc" ;
  setColorStrokingICC = @"SCN" ;
  setColorNonStrokingICC = @"scn" ;
  paintShadingPattern = @"sh" ;
  moveStartText = @"T*" ;
  setCharSpacing = @"Tc" ;
  moveTextPos = @"Td" ;
  moveTextPosSetLeading = @"TD" ;
  setTextFont = @"Tf" ;
  showText = @"Tj" ;
  showTextIndGlyph = @"TJ" ;
  setTextLeading = @"TL" ;
  setTextMatrix = @"Tm" ;
  setTextRendering = @"Tr" ;
  setTextRise = @"Ts" ;
  setWordSpacing = @"Tw" ;
  setHorizontalTextScaling = @"Tz" ;
  appendCurvedInitPtRepl = @"v" ;
  setLineWidth = @"w" ;
  setClippingNzWinding = @"W" ;
  setClippingEvenOdd = @"W*" ;
  appendCurvedFinalPt = @"y" ;
  moveShow = @"'" ;
  setSpacing = @"\"" ;
}

-- OpObj: an object built from an operation (see Name parser)
def OperationObj = Token ContentStreamOp

-- BeginCompat: marks the beginning of a compatibility section
def BeginCompat = Token "BX"

-- EndCompat: marks the end of a compatibility section
def EndCompat = Token "EX"

-- OpName: an object built from a valid operation name, which may not
-- be a recognized operation.
def OpName = Token { Many NameChar }

-- ContentStream: parse a content stream
def ContentStream = {
  Many {
    Choose {
      operand = Value ;
      operation = OperationObj ;

      -- compatibility section: a beginning marker, sequence of any
      -- object or valid operation name, and an end marker
      compatSect = {
        BeginCompat ;
        Many (@Value | @OpName) ;
        EndCompat ;
      }
    }
  }
}

--------------------------------------------------------------------------------
-- Section 7.9.3: text sterams

-- Parsers for validating text objects:
def Byte = 0 .. 255

-- U+001B: the Unicode escape character
def U001B = { 0 ; 27 }

def HighSurrogate = {
  0xd8 .. 0xdb ; 
  Byte
}

def LowSurrogate = {
  0xdc .. 0xdf ; 
  Byte
}

-- BmpByte: a non-surrogate byte
def BmpByte = 0x00 .. 0xd7 | 0xe0 .. 0xff

def UniChar = Choose1 {
  escape = {
    U001B ;

    -- an ISO language code:
    lang = LanguageCode ;

    -- an optional ISO country code:
    ctry = Optional CountryCode ;

    U001B 
  } ;

  supplementary = {
    big = HighSurrogate ;
    small = LowSurrogate
  } ;

  twoByte = {
    big = BmpByte ;
    small = BmpByte
  } ;
}

def UFEFF = { 254 ; 255 }

-- TextObj: parses a text object
def TextObj = {
  Choose1 {
    -- validate as unicode
    isUnicode = {
      UFEFF ; -- magic number from Sec. 7.9.2.2
      Many UniChar 
    } ;
  
    -- validate as PDFDocEncoding (arbitrary bytes)
    isPdfDoc = Many Byte ;
  } ;

  END
}

--------------------------------------------------------------------------------
-- Section 7.9.4: dates

def BoundedTwoDigits lb ub = {
  @digs = Many 2 Digit ;
  $$ = ^ numBase 10 digs ;
  lb <= $$ ; $$ <= ub
}

def Month = BoundedTwoDigits 1 12

def Minute = BoundedTwoDigits 0 60

def Hour = BoundedTwoDigits 0 60

def Second = BoundedTwoDigits 0 60

def Day = BoundedTwoDigits 1 31

def Date = {
    "D:" ;
    year = Many 4 Digit ;
    month = Month ;
    day = Day ;
    hour = Hour ;
    minute = Minute ;
    second = Second ;
    relLocalTime = Choose {
      plus = @'+' ;
      minus = @'-' ;
      bigZ = @'Z' ;
    } ;
    utOffsetHours = Hour ;
    '\'' ;
    utOffsetMins = Minute ;
}


