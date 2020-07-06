--------------------------------------------------------------------------------
-- The declarations in this module are all related to Section 7
--------------------------------------------------------------------------------

import ISOCodes

--------------------------------------------------------------------------------
-- White Space (Section 7.2)

def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { "%"; Many (! ($lf | $cr)); EOL }
def AnyWS                 = $simpleWS | Comment | EOL
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Helpers

def Token P               = { $$ = P; Many AnyWS }
def KW P                  = @ (Token P)
def Between open close P  = { KW open; $$ = P; KW close }
def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def Only P                = { $$ = P; END }
def When P x              = { P; ^ x }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Boolean Objects (Section 7.3.2)

def Bool =
    When (KW "true")  true
  | When (KW "false") false


--------------------------------------------------------------------------------
-- Numberic Objects (Section 7.3.3)

def Number = Token {
  @sign = Sign;
  @n    = UnsignedNumber;
    When (sign is pos) n
  | When (sign is neg) { num = 0 - n.num; exp = n.exp }
}

def Sign = Choose {
  pos = @("+" | "");
  neg = @"-"
}

def UnsignedNumber =
    UnsignedLeadDigits
  | Frac 1 { num = 0, exp = 0 }

def UnsignedLeadDigits = {
  @n   = Natural;
  @val = ^ { num = n; exp = 0 : int };
  Frac 0 val  <| (^ val)
  -- biased choice here is important
  -- otherwise 1.2 can be processed as "1.2" or "1" followed by ".2"
}

def Natural = {
  @ds = Many (1..) Digit;
  ^ numBase 10 ds
}

def Frac n (w : Number) : Number = {
  @ds = { "."; Many (n ..) Digit };
  ^ for ( val = w; d in ds)
          { num = 10 * val.num + d; exp = val.exp - 1 }
}

def Digit     = { @d = '0' .. '9'; ^ d - '0' as int }
def OctDigit  = { @d = '0' .. '7'; ^ d - '0' as int }
def HexDigit  = Digit
              | { @d = 'a' .. 'f'; ^ 10 + (d - 'a' as int) }
              | { @d = 'A' .. 'F'; ^ 10 + (d - 'A' as int) }

def NumberAsNat (x : Number) = { x.num >= 0; x.exp == 0; ^ x.num }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Literal Strings (Section 7.3.4.2)

def String = Between "(" ")" StringChars

def StringChars = concat (Many StringChunk)

def StringChunk =
    StringInParens
  | StringEsc
  | Many (1..) (! "\\()")

def StringInParens = concat [ "(", StringChars, ")"]

def StringEsc = {
   "\\";
   Choose {
     When "n"   "\n";
     When "r"   "\r";
     When "t"   "\t";
     When "b"   "\8";
     When "f"   "\12";
     When "("   "(";
     When ")"   ")";
     When "\\"  "\\";
     When EOL   "";
     StringNumEsc
  }
}

def StringNumEsc = {
  @ds = Many (1..3) OctDigit;
  ^ [ numBase 8 ds as! uint 8 ]
}



--------------------------------------------------------------------------------
-- Hexadecimal Strings (Sections 7.3.4.3)

def HexString = Between "<" ">" {
  @front = Many HexStringNum2;
    concat [ ^ front, [HexStringNum1] ]
  | ^ front
}

def HexStringNum2 = {
  @ds = Many 2 (Token HexDigit);
  ^ numBase 16 ds as! uint 8
}

def HexStringNum1 = {
  @d = Token HexDigit;
  ^ 16 * d as! uint 8;
}


--------------------------------------------------------------------------------
-- Name Objectcs (Sections 7.3.5)

def Name     = Token { "/"; Many NameChar }

def NameChar = !"\0\9\10\12\13\32()<>[]{}/%#"
             | NameEsc

def NameEsc  = {
  "#";
  @ds = Many 2 HexDigit;
  $$  = ^ numBase 16 ds as! uint 8;
  $$ > 0
}



--------------------------------------------------------------------------------
-- Array Objectcs (Section 7.3.6)

def Array = Between "[" "]" (Many Value)


--------------------------------------------------------------------------------
-- Dictionary Objects (Section 7.3.7)

def Dict = {
  @ents = Between "<<" ">>" (Many { key = Name; value = Value });
  for (d = empty; e in ents) (Insert e.key e.value d)
}


def Null  = KW "null"

def Ref = {
  obj = Token Natural;
  gen = Token Natural;
  KW "R"
}


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

-- Objects ---------------------------------------------------------------------

def Value =
  Choose1 {
    null    = Null;
    bool    = Bool;
    ref     = Ref;      -- This must come before number, as they overlap
    name    = Name;
    string  = String;
    string  = HexString;
    number  = Number;
    array   = Array;
    dict    = Dict;
  }

def NatValue (v : Value) = {
  @n = v is number;
  NumberAsNat n;
}

def nullValue : Value = {| null = {} |}

