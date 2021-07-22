
import Stdlib
import PdfValue  -- TODO: need to replace with GenPdfValue??
import PdfDecl   

import Type1Font -- TODO: split out dependent defns

-- the type 0 font dictionary, see spec p. 341
def Type0Font = {
  @d = Dict;
  @ty = LookupName "Type" d;
  Guard (ty == "Font");
  
  @subty = LookupName "SubType" d;
  Guard (subty == "Type0");
    
  basefont  = LookupName "BaseFont" d;
  
  @enc = Lookup "Encoding" d;

  encoding = Choose1 {
                {@v = ResolveVal enc; @n = v is name; ^ {| primName = n |} };
                {@s = ResolveStream enc; ^ {| streamEnc = s |}};
              };
  
              -- name or stream
              -- not the same as 'Encoding' for other Type1,...
  
  descendants = LookupResolve "DescendantFonts" d; -- one element array
  
  toUnicode = Optional (LookupResolve "ToUnicode" d);
  }

def Type0FontP = Type0Font
