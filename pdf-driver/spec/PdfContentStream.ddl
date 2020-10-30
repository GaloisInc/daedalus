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
def ContentStreamOp = Choose1 {
  closeFillStrokeEvenOdd = KW "b*" ;
  closeFillStrokeNzWinding = KW "b" ;
  fillStrokeEvenOdd = KW "B*" ;
  beginMarkedContent = KW "BDC" ;
  beginInline = KW "Bl" ;
  beginMarkedContent = KW "BMC" ;
  beginText = KW "BT" ;
  fillStroke = KW "B" ;
  concatMatrix = KW "cm" ;
  setColorSpaceStroking = KW "CS" ;
  setColorSpaceNonStroking = KW "cs" ;
  appendCurvedThreePoints = KW "c" ;
  setGlyphWidth = KW "d0" ;
  setGlpyhWidthBoundingBox = KW "d1" ;
  setLineDash = KW "d" ;
  invokeXObj = KW "Do" ;
  defMarkedContentPoint = KW "DP" ;
  endInline = KW "El" ;
  endMarkedContent = KW "EMC" ;
  endTextObj = KW "ET" ;
  fillPathEvenOdd = KW "f*" ;
  fillPathNzWinding = KW "f" ;
  fillPathNzWindingOld = KW "F" ;
  setGrayStroking = KW "G" ;
  setGraphicsStateParams = KW "gs" ;
  setGrayNonStroking = KW "g" ;
  closeSubpath = KW "h" ;
  setFlat = KW "i" ;
  beginInlineImageData = KW "ID" ;
  setLineJoinStyle = KW "j" ;
  setLineCapStyle = KW "J" ;
  setCMYKStroking = KW "K" ;
  setCMYKNonStroking = KW "k" ;
  appendLine = KW "l" ;
  beginNewSuppath = KW "m" ;
  defineMarkedContent = KW "MP" ;
  setMiterLimit = KW "M" ;
  endPath = KW "n" ;
  saveGraphicsState = KW "q" ;
  restoreGraphicsState = KW "Q" ;
  setRGBStroking = KW "RG" ;
  appendRect = KW "re" ;
  setRGBNonStroking = KW "rg" ;
  setColorRenderingIntent = KW "ri" ;
  setColorNonStrokingICC = KW "scn" ;
  setColorNonStroking = KW "sc" ;
  closeStrokePath = KW "s" ;
  setColorStrokingICC = KW "SCN" ;
  setColorStroking = KW "SC" ;
  stroke = KW "S" ;
  paintShadingPattern = KW "sh" ;
  moveStartText = KW "T*" ;
  setCharSpacing = KW "Tc" ;
  moveTextPos = KW "Td" ;
  moveTextPosSetLeading = KW "TD" ;
  setTextFont = KW "Tf" ;
  showText = KW "Tj" ;
  showTextIndGlyph = KW "TJ" ;
  setTextLeading = KW "TL" ;
  setTextMatrix = KW "Tm" ;
  setTextRendering = KW "Tr" ;
  setTextRise = KW "Ts" ;
  setWordSpacing = KW "Tw" ;
  setHorizontalTextScaling = KW "Tz" ;
  appendCurvedInitPtRepl = KW "v" ;
  setLineWidth = KW "w" ;
  setClippingEvenOdd = KW "W*" ;
  setClippingNzWinding = KW "W" ;
  appendCurvedFinalPt = KW "y" ;
  moveShow = KW "'" ;
  setSpacing = KW "\"" ;
}

-- OperationObj: an object built from a known content stream operation
def ContentStreamOpObj = Token ContentStreamOp

-- OpName: an object built from a valid operation name, which may not
-- be a recognized operation.
def OpName = Token { NameChar ; Many NameChar }

-- ContentOp: either a known or a future operand
def ContentOp = Choose1 {
  knownOp = ContentStreamOpObj ;
  futureOp = OpName ;
}

-- BeginCompat: marks the beginning of a compatibility section
def BeginCompat = KW "BX"

-- EndCompat: marks the end of a compatibility section
def EndCompat = KW "EX"

-- ContentStreamBody Op: parse the body of a content stream, using Op
def ContentStreamBody Op = {
  Many AnyWS ;
  Many {
    Choose1 {
      -- compatibility section: a beginning marker, sequence of operands
      -- and operations that may not be known names, and an end marker
      compatSect = {
        BeginCompat ;
        commit;
        ContentStreamBody ContentOp ;
        EndCompat ;
      } ;

      operand = ContentStreamOperandObj ; 
      operation = Op ;
    }
  }
}

-- ContentStream: data that forms a content stream
def ContentStream = ContentStreamBody {| knownOp = ContentStreamOpObj |}
