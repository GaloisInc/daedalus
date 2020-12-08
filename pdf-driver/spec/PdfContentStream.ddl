import PdfValue

-- general purpose helpers (TODO: some are clones):

-- Byte: any byte
def Byte = 0 .. 255

def Default2 x P = P <| ^ x

-- Bot P: always fails to parse a P
def Bot P = { Guard false ; P }

-- GenArray P: general array of P's
def GenArray P = Between "[" "]" (Many P)

def KeyObjsToMap ents = 
  for (d = empty; e in ents) (Insert e.key e.obj d)

def GenDict Obj = {
  @ents = Between "<<" ">>" (Many { key = Name; obj = Obj });
  KeyObjsToMap ents
}

-- Cmp a b: parser that evaluates a <= b and succeeds iff true
def Cmp a b = 
  Guard (a.num < b.num) |
  { Guard (a.num == b.num) ;
    Guard (a.exp <= b.exp)
  }

-- Fractional: parses Number n s.t. 0 <= n < 1
def Fractional = {
  $$ = Number ;
  Cmp { num = ^ 0 ; exp = ^ 0 } $$ ;
  Cmp $$ { num = ^ 1 ; exp = ^ 0 } 
}

-- BoundedNonNeg ub: parses a number n s.t. n <= ub
def BoundedNonNeg ub = {
  $$ = UnsignedNumber ;
  Cmp $$ ub
}

-- GenDirectObj Obj: a direct object over objects Obj
def GenDirectObj Obj = Choose1 {
  null    = Null;
  bool    = Bool;
  name    = Name;
  string  = String;
  string  = HexString;
  number  = Number;
  array   = GenArray Obj;
  dict    = GenDict Obj;
}

-- DirectObj: object made up purely of direct objects:
def DirectObj = {
  box = GenDirectObj DirectObj
} 

def DirectArray = GenArray DirectObj

def DirectDict = GenDict DirectObj

--------------------------------------------------------------------------------

-- Section 7.8.2: content streams:

-- ContentProps: properties for a content stream object
def ContentProps = Choose {
  inline = Dict ;
  nm = Name ; -- TODO: refine to check resource dictionary
}

-- ContentPoint: a coordinate, represented as a pair of numbers
def ContentPoint = {
  x = Token Number ;
  y = Token Number ;
}

def LineStyle = Match1 ('0' .. '2')

-- Intent: intents
def Intent = Choose1 {
  absoluteColorimetric = "/Absolutecolorimetric" ;
  relativeColorimetric = "/RelativeColorimetric" ;
  saturation = "/Saturation" ;
  perceptual = "/Perceptual" ;
}

-- Path-Painting Operations (Table 60)
def PathPaintingOp = Choose1 {
  stroke = KW "S" ;
  closeStrokePath = KW "s" ;
  fillPathNzWindingOld = KW "F" ;
  fillPathEvenOdd = KW "f*" ;
  fillPathNzWinding = KW "f" ;
  fillStrokeEvenOdd = KW "B*" ;
  fillStroke = KW "B" ;
  closeFillStrokeEvenOdd = KW "b*" ;
  closeFillStrokeNzWinding = KW "b" ;
  endPath = KW "n" ;
}

-- Marked-content operators (Table 320)
def MarkedContentOp = Choose1 {
  defineMarkedContent = {
    tag = Token Name ;
    KW "MP" ;
  } ;
  defMarkedContentPoint = {
    tag = Token Name ;
    props = Token ContentProps ;
    KW "DP" ;
  } ;
}

-- A marked-content sequence (Sec. 14.6)
def MarkedContentSeq FutureOp = {
  beginMarked = Choose1 { -- begin the marked content
    beginMarkedContent = KW "BMC" ;
    beginMarkedContentProp = {
      tag = Token Name ;
      props = Token ContentProps ;
      KW "BDC" ;
    } ; 
  } ;

  page = PageDescription FutureOp ;

  KW "EMC" ; -- end the marked content
}

def PathBeginOp = Choose1 {
  -- begin the path object:
  beginNewSuppath = {
    pt = Token ContentPoint ;
    KW "m" ;
  } ;
  appendRect = {
    pt = Token ContentPoint ;
    width = Token Number ;
    height = Token Number ;
    KW "re" ;
  } ;
}

-- Path Construction Operators (Table 59)
def PathConsOp = Choose1 {
  beginSubpath = PathBeginOp ;
  appendLine = {
    pt = Token ContentPoint ;
    KW "l" ;
  } ;
  appendCurvedThreePoints = {
    pt1 = Token ContentPoint ;
    pt2 = Token ContentPoint ;
    pt3 = Token ContentPoint ;
    KW "c" ;
  } ;
  appendCurvedInitPtRepl = {
    pt2 = Token ContentPoint ;
    pt3 = Token ContentPoint ;
    KW "v" ;
  } ;
  appendCurvedFinalPt = {
    pt1 = Token ContentPoint ;
    pt3 = Token ContentPoint ;
    KW "y" ;
  } ;
  closeSubpath = KW "h" ;
}

-- General Graphics State operators (Tables 51, 57)
def GenGraphicsStateOp = Choose1 {
  setLineWidth = {
    lineWidth = Token Number ;
    KW "w" ;
  } ;
  setLineCapStyle = {
    lineCap = Token LineStyle ;
    KW "J" ;
  } ;
  setLineJoinStyle = {
    lineJoin = Token LineStyle ;
    KW "j" ;
  } ;
  setMiterLimit = {
    miterLimit = Token Number ;
    KW "M" ;
  } ;
  setLineDash = {
    dashArray = Token (GenArray (Token Natural)) ; -- TODO: check that some array elt is non-zero
    dashPhase = Token Number ;
    KW "d" ;
  } ;
  setColorRenderingIntent = {
    intent = Token Intent ;
    KW "ri" 
  } ;
  setFlat = {
    flatness = Token (BoundedNonNeg { num = ^ 100 ; exp = ^ 0 }) ;
    KW "i" ;
  } ;
  setGraphicsStateParams = {
    dictName = Token Name ;
    KW "gs" ;
  }
}

-- Graphics State Operators (Table 57)
def SpecialGraphicsStateSeq FutureOps = Choose1 {
  nesting = {
    KW "q" ; -- push a copy of the graphics state

    page = PageDescription FutureOps ;

    KW "Q" -- restore the graphics stack:
  } ;

  concatMatrix = {
    a = Token Number ;
    b = Token Number ;
    c = Token Number ;
    d = Token Number ;
    e = Token Number ;
    f = Token Number ;
    KW "cm" ;
  } ;
}

-- Color spaces (Sec. 8.6)
def ColorSpace = Choose1 {
  -- CIE-based color spaces:
  calRGB = "/CalRGB" ;
  calGray = "CalGray" ;
  lab = "/Lab" ;
  iccBased = "/ICCBased" ;
  
  -- Device color spaces:
  deviceRGB = "/DeviceRGB" ;
  deviceCMYK = "/DeviceCMYK" ;
  deviceGray = "/DeviceGray" ;

  -- Special color spaces:
  sep = "/Separation" ;
  devN = "/DeviceN" ;
  indexed = "/Indexed" ;
  pattern = "/Pattern" ;
}

-- Colour Operators (Table 74)
-- TODO: track color spaces that have been set
def ColourOp = Choose1 {
  setColorSpaceStroking = {
    nm = Token Name ;
    KW "CS" ;
  } ;
  setColorSpaceNonStroking = {
    nm = Token Name ;
    KW "cs" ;
  };
  setColorStroking = {
    -- TODO: fully parsing this requires tracking the current color space
    cs = Many (Token Number) ;
    KW "SC" 
  } ;
  setColorStrokingICC = {
    cs = Many (Token Number) ;
    KW "SCN" 
  } ;
  setColorNonStroking = {
    -- TODO: fully parsing this requires tracking the current color space
    cs = Many (Token Number) ;
    KW "sc" 
  } ;
  setColorNonStrokingICC = {
    -- TODO: fully parsing this requires tracking the current color space
    cs = Many (Token Number) ;
    KW "scn" 
  } ;
  setGrayStroking = {
    gray = Token Fractional ;
    KW "G" ;
  } ;
  setGrayNonStroking = {
    gray = Token Fractional ;
    KW "g" ;
  } ;
  setRGBStroking = {
    r = Token Fractional ;
    g = Token Fractional ;
    b = Token Fractional ;
    KW "RG" ;
  } ;
  setRGBNonStroking = {
    r = Token Fractional ;
    g = Token Fractional ;
    b = Token Fractional ;
    KW "rg" 
  } ;
  setCMYKStroking = {
    c = Token Fractional ;
    m = Token Fractional ;
    y = Token Fractional ;
    k = Token Fractional ;
    KW "K" 
  } ;
  setCMYKNonStroking = {
    c = Token Fractional ;
    m = Token Fractional ;
    y = Token Fractional ;
    k = Token Fractional ;
    KW "K" 
  } ;
}

-- Type 3 font operators (Table 113)
def FontOp = Choose1 {
  setGlyphWidth = {
    wx = Token Number ;
    Token (Match1 '0') ;
    KW "d0" 
  } ;
  setGlpyhWidthBoundingBox = {
    wx = Token Number ;
    Token (Match1 '0') ;
    llx = Token Number ;
    lly = Token Number ;
    urx = Token Number ;
    ury = Token Number ;
    Token (Match1 '0') ;
    KW "d1" 
  } ;
} 

-- Text-positioning operators (Table 108)
def TextPosOp = {
  moveTextPos = {
    tx = Token Number ;
    ty = Token Number ;
    KW "Td" 
  } ;
  moveTextPosSetLeading = {
    tx = Token Number ;
    ty = Token Number ;
    KW "TD" 
  } ;
  setTextMatrix = {
    a = Token Number ;
    b = Token Number ;
    c = Token Number ;
    d = Token Number ;
    e = Token Number ;
    f = Token Number ;
    KW "Tm" 
  } ;
  moveStartText = KW "T*" ;
}

-- Text state operators (Table 105)
def TextStateOp = Choose1 {
  setCharSpacing = {
    charSpace = Token Number ;
    KW "Tc"
  } ;
  setWordSpacing = {
    x = Token Number ;
    KW "Tw" 
  } ;
  setHorizontalTextScaling = {
    scale = Token Number ;
    KW "Tz" 
  } ;
  setTextLeading = {
    leading = Token Number ;
    KW "TL" 
  } ;
  setTextFont = {
    font = Token Name ;
    size = Token Number ;
    KW "Tf" 
  } ;
  setTextRendering = {
    render = Token Natural ;
    KW "Tr" 
  } ;
  setTextRise = {
    rise = Token Number ;
    KW "Ts" 
  } ;
}

-- ShowVal: a value that can be shown
def ShowVal = Choose1 {
  str = String ;
  num = Number ;
}

-- Text-showing operators (Table 109)
def TextShowOp = Choose1 {
  showText = {
    str = Token String ;
    KW "Tj" 
  } ;
  moveShow = {
    str = Token String ;
    KW "'" 
  } ;
  setSpacing = {
    aw = Token Number ;
    ac = Token Number ;
    KW "\"" 
  } ;
  showTextIndGlyph = {
    arr = Token (GenArray (Token ShowVal)) ;
    KW "TJ" 
  } ;
} 

-- Clipping Path Operators (Table 61)
def ClippingPathOp = Choose1 {
  setClippingEvenOdd = KW "W*" ;
  setClippingNzWinding = KW "W" ;
}

-- Text Objects (Sec. 9.4)
def TextObj FutureOp = {
  KW "BT" ; -- begin text object

  ops = Many (Choose1 { -- allowed operations:
    graphicsStateOp = GenGraphicsStateOp ;
    color = ColourOp ;
    textState = TextStateOp ;
    textPos = TextPosOp ;
    textShow = TextShowOp ;
    marked = MarkedContentSeq FutureOp ;
  }) ;

  KW "ET" -- end text object
} 

-- ShadingObj: a shading object
def ShadingObj = {
  name = Token Name ;
  KW "sh" ;
}

-- XObj: a external object
def XObj = {
  name = Token Name ;
  KW "Do" ;
}

-- LookupDirectNat: lookup a nat, given as a direct object
def LookupDirectNat k m =
  { @vV = Lookup k m : DirectObj;
    @v  = vV.box is number;
    NumberAsNat v; 
  }

-- LastElt fs: the last element in fs
def LastElt fs = for (last = nothing; f in fs) (just f)

-- Inline Image Operators (Table 92)
def InlineImageObj = {
  KW "BI" ; -- begin image

  -- a sequence of key-value pairs
  hdrEnts = Many {
    key = Token Name ;
    Choose1 { -- Entries in Table 91:
      bpc = { -- Bits Per Component:
        Guard (key == "BitsPerComponent" || key == "BPC") ;
        Natural ;
      } ;
      colorSpace = {
        Guard (key == "ColorSpace" || key == "CS") ;
        Choose1 { -- Color Space:
          nm = Name ;
          arr = DirectArray ;
        } ;
      } ;
      decode = {
        Guard (key == "Decode" || key == "D") ;
        GenArray Number ;
        -- TODO: check value of ImageMask (see Table 87)
      } ;
      decodeParams = {
        Guard (key == "DecodeParms" || key == "DP") ;
        Choose1 {
          d = DirectDict ;
          arr = GenArray DirectDict;
        }
        -- TODO: check value associated with Filter (see Table 5)
      } ;
      filter = {
        Guard (key == "Filter" || key == "F") ;
        Choose1 {
          name = Name ;
          names = GenArray Name ;
        }
      } ;
      height = {
        Guard (key == "Height" || key == "H") ;
        Natural 
      } ;
      imageMask = {
        Guard (key == "ImageMask" || key == "IM") ;
        Bool
      } ;
      intent = {
        Guard (key == "Intent") ;
        Choose1 { -- Table 69:
          absColorimetric = "AbsoluteColorimetric" ;
          relColorimetric = "RelativeColorimetric" ;
          sat = "Saturation" ;
          perceptual = "Perceptual" ;
        }
      } ;
      interpolate = {
        Guard (key == "Interpolate" || key == "I") ;
        Bool
      } ;
      len = {
        Guard (key == "Length" || key == "L") ;
        Natural 
      } ;
      width = {
        Guard (key == "Width" || key == "W") ;
        Natural
      }
    } ;
    value = Token DirectObj
  } ;
  hdr = for (d = empty; e in hdrEnts) (Insert e.key e.value d) ;

  -- TODO: validate all keys in the dictionary

  -- TODO: this validates exactly PDF 2.0. Relax to compute from
  -- fields used in previous versions?

  -- TODO: update to be record of options, then coerce
  len = (LookupDirectNat "Length" hdr) | (LookupDirectNat "L" hdr) ;
  Guard (len <= 4096) ;

  KW "ID" ; $simpleWS; -- begin image data
  Default2 [ ] {
    @filtersObj = (Lookup "Filter" hdr) | (Lookup "F" hdr) ;
    @filters = filtersObj.box is array ;
    -- TODO: coerce filters to list of names
    @filterNames = map (fobj in filters) {
      fobj.box is name 
    } ;
    @lastFilter = LastElt filterNames ;
    @lastF = lastFilter is just ;
    Guard (lastF == "ASCIIHexDecode" || lastF == "ASCII85Decode") ;
    Many AnyWS
  } ;

  imageData = Many len Byte ;
  Many AnyWS;
  KW "EI" ; -- end image
} 

-- PathObj: a path object:
def PathObj = {
  -- begin the path object:
  begin = PathBeginOp ;

  -- path construction operators:
  pathOps = Many PathConsOp ;

  clippy = Optional ClippingPathOp ;

  paint = PathPaintingOp ; -- ends the path object
}

-- PossibleOp: a valid lexical op, which could be valid in a future version
def PossibleOp = {
  operands = Many (Token DirectObj) ;
  op = Token (Many NameChar) ;
}

-- CompatOps: 
def CompatOps FutureOp = Choose1 {
  compatSection = {
    KW "BX" ; -- beginning of a compatibility section:

    -- page description, accepting any operand:
    $$ = PageDescription PossibleOp ; 

    KW "EX" -- ending of the campatibility section:
  } ;

  futureOp = FutureOp ; -- a future op:
}

-- PageDescription (Fig. 9): a sequence of:
def PageDescription FutureOp = Many (Choose1 {
  -- allowed individual operations:
  genGraphics = GenGraphicsStateOp ;
  specialGraphics = SpecialGraphicsStateSeq FutureOp ;
  colour = ColourOp ;
  textState = TextStateOp ;
  markedContent = MarkedContentSeq FutureOp ;

  -- graphics objects:
  textObj = TextObj FutureOp ;
  shadingObj = ShadingObj ;
  xObj = XObj ;
  inlineObj = InlineImageObj ;
  pathObj = PathObj ;

  -- compatibility sections:
  compat = CompatOps FutureOp ;
})

-- ContentStream: a page description, prefixed by whitespace
def ContentStream = {
  Many AnyWS ;
  -- page description, accepting no "future" operands
  PageDescription (Bot PossibleOp) ;
}
