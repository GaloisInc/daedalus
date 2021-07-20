import Stdlib

import Type1Font
import Type0Font

-- TODO: replace with property defns
def Type3Font = Void

def TrueTypeFont = Void

def CIDFont = Void

def FontDict = Choose1 { 
  -- fonts are mutually exclusive, due to at least the Subtype field
  --  these all dictionaries, all contain << /Type /Font >>
  type0 = Type0Font;
  type1 = Type1Font;
  type3 = Type3Font;
  trueType = TrueTypeFont;
  cidFont = CIDFont;
}
