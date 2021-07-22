import Stdlib
import Pair

import Type1Font
import Type0Font

-- TODO: indirect deps
import PdfValue
import CMap

-- TODO: replace with property defns
def Type3Font = Void

def TrueTypeFont = Void

def CIDFont = Void

def FontDict = Choose1 { 
  -- fonts are mutually exclusive, due to at least the Subtype field
  --  these all dictionaries, all contain << /Type /Font >>
  type0 = Type0Font;
  type1 = Type1FontP;
  type3 = Type3Font;
  trueType = TrueTypeFont;
  cidFont = CIDFont;
}
