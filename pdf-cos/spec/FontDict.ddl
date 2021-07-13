-- Font: a font
def Type0Font = Void

-- Type 1 font: encoding of Table 109
def Type1Font = {
  @fontDict = Type1Font0;
  FinalizeFont0 fontDict
}

def Type3Font = Void

def TrueTypeFont = Void

def CIDFont = Void

def Font = Choose1 { 
  -- fonts are mutually exclusive, due to at least the Type field
  type0 = Type0Font;
  type1 = Type1Font;
  type3 = Type3Font;
  trueType = TrueTypeFont;
  cidFont = CIDFont;
}
