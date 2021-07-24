-- Colour space operators
import PdfValue

-- Colour Operators (Table 73)
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
    gray = Token UnsignedNumber ;
    KW "G" ;
  } ;
  setGrayNonStroking = {
    gray = Token UnsignedNumber ;
    KW "g" ;
  } ;
  setRGBStroking = {
    r = Token UnsignedNumber ;
    g = Token UnsignedNumber ;
    b = Token UnsignedNumber ;
    KW "RG" ;
  } ;
  setRGBNonStroking = {
    r = Token UnsignedNumber ;
    g = Token UnsignedNumber ;
    b = Token UnsignedNumber ;
    KW "rg" 
  } ;
  setCMYKStroking = {
    c = Token UnsignedNumber ;
    m = Token UnsignedNumber ;
    y = Token UnsignedNumber ;
    k = Token UnsignedNumber ;
    KW "K" 
  } ;
  setCMYKNonStroking = {
    c = Token UnsignedNumber ;
    m = Token UnsignedNumber ;
    y = Token UnsignedNumber ;
    k = Token UnsignedNumber ;
    KW "k" 
  } ;
}
