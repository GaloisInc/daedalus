import PdfValue

--------------------------------------------------------------------------------

-- Section 7.8.2: content streams:

-- DirectObj: direct objects: all objects except for refs.
def DirectObj = Choose1 {
  null    = Null;
  bool    = Bool;
  name    = Name;
  string  = String;
  string  = HexString;
  number  = Number;
  array   = Array;
  dict    = Dict;
}

def ContentStreamOperandObj = Token DirectObj

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

-- OperationObj: an object built from a known content stream operation
def ContentStreamOpObj = Token ContentStreamOp

-- OpName: an object built from a valid operation name, which may not
-- be a recognized operation.
def OpName = Token { Many NameChar }

-- ContentOp: either a known or a future operand
def ContentOp = Choose {
  knownOp = ContentStreamOpObj ;
  futureOp = OpName ;
}

-- BeginCompat: marks the beginning of a compatibility section
def BeginCompat = Token "BX"

-- EndCompat: marks the end of a compatibility section
def EndCompat = Token "EX"

-- ContentStreamBody Op: parse the body of a content stream, using Op
def ContentStreamBody Op = Many {
  Choose {
    operand = ContentStreamOperandObj ; 
    operation = ContentStreamOpObj ;

    -- compatibility section: a beginning marker, sequence of operands
    -- and operations that may not be known names, and an end marker
    compatSect = {
      BeginCompat ;
      ContentStreamBody ContentOp ;
      EndCompat ;
    }
  }
}

-- ContentStream: data that forms a content stream
def ContentStream = ContentStreamBody {| knownOp = ContentStreamOpObj |}
