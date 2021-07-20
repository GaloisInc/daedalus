import Stdlib

import Type1Font

-- TODO: indirect deps
import PdfValue
import CMap

-- TODO: replace with property defns
def Type0Font = Void

def Type3Font = Void

def TrueTypeFont = Void

def CIDFont = Void

def FontDict = Choose1 { 
  -- fonts are mutually exclusive, due to at least the Type field
  type0 = Type0Font;
  type1 = Type1Font;
  type3 = Type3Font;
  trueType = TrueTypeFont;
  cidFont = CIDFont;
}
