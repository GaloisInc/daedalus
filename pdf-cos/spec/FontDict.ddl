import Stdlib
import Pair

import Type1Font
import Type0Font

-- TODO: indirect deps
import PdfValue
import CMap
import CIDFont

-- TODO: replace with defn in Type3 module, refactor with Type0 and Type1
def Type3Font = Void

def TrueTypeFont = Void

def FontDict = Choose1 { 
  -- fonts are mutually exclusive, due to at least the Subtype field
  --  these all dictionaries, all contain << /Type /Font >>
  type0 = Type0FontP;
  type1 = Type1FontP;
--  type3 = Type3Font;
--  trueType = TrueTypeFont;
}

def MkType0Font (f0 : Type0Font) : FontDict = {| type0 = f0 |}
