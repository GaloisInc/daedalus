import Stdlib
import Pair

import Type0Font
import Type1Font
import Type3Font

-- TODO: indirect deps
import CMap
import CIDFont
import Encoding
import PdfValue

def FontDict = Choose1 { 
  -- fonts are mutually exclusive, due to at least the Subtype field
  --  these all dictionaries, all contain << /Type /Font >>
  type0 = Type0FontP;
  type1 = Type1FontP;
  mmfont = MMFontP;
  type3 = Type3FontP;
--  trueType = TrueTypeFont; TODO: define
}

def MkType0Font (f0 : Type0Font) : FontDict = {| type0 = f0 |}
