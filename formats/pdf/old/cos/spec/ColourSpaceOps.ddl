-- Colour space operators
import PdfValue

-- Colour Operators (Table 73)
-- TODO: order by some anti-prefix order
def ColourOp = First
  setColorSpaceStroking = {
    nm = Token Name ;
    KW "CS" ;
    }
  setColorSpaceNonStroking = {
    nm = Token Name ;
    KW "cs" ;
    }
  setColorStrokingICC = {
    cs = Many (Token Number) ;
    KW "SCN" 
    }
  setColorStroking = {
    -- TODO: fully parsing this requires tracking the current color space
    cs = Many (Token Number) ;
    KW "SC" 
    }
  setColorNonStrokingICC = {
    -- TODO: fully parsing this requires tracking the current color space
    cs = Many (Token Number) ;
    KW "scn" 
    }
  setColorNonStroking = {
    -- TODO: fully parsing this requires tracking the current color space
    cs = Many (Token Number) ;
    KW "sc" 
    }
  setGrayStroking = {
    gray = Token UnsignedNumber ;
    KW "G" ;
    }
  setGrayNonStroking = {
    gray = Token UnsignedNumber ;
    KW "g" ;
    }
  setRGBStroking = {
    r = Token UnsignedNumber ;
    g = Token UnsignedNumber ;
    b = Token UnsignedNumber ;
    KW "RG" ;
    }
  setRGBNonStroking = {
    r = Token UnsignedNumber ;
    g = Token UnsignedNumber ;
    b = Token UnsignedNumber ;
    KW "rg" 
    }
  setCMYKStroking = {
    c = Token UnsignedNumber ;
    m = Token UnsignedNumber ;
    y = Token UnsignedNumber ;
    k = Token UnsignedNumber ;
    KW "K" 
    }
  setCMYKNonStroking = {
    c = Token UnsignedNumber ;
    m = Token UnsignedNumber ;
    y = Token UnsignedNumber ;
    k = Token UnsignedNumber ;
    KW "k" 
    }

