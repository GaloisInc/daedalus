-- Harness: testing harness
import ResourceDict
import TextEffect

import FontCommon
import FontDict
import Type0Font
import Type1Font
import FontDesc
import TextObj
import ContentStreamLight

def TestFont : FontDict = MkType0Font (Type0Font (PartialType0Font
  (PartialCommonFont
    true
    true
    nothing)
  (just Helvetica)
  (just (PreDefEncoding "TestEnc"))
  nothing))

def TestResrcs : ResourceDict = ResourceDict (PartialResourceDict
  nothing
  nothing
  nothing
  nothing
  nothing
  (just (Insert "F13" TestFont empty))
  nothing
  nothing)

def TestSizedFont = SizedFont TestFont 12

-- Main: the entry point
def Main = ExtractContentStreamText (ContentStreamP TestResrcs)

-- TODO: test:
-- pages
-- page tree nodes

-- text extraction: properly support Type0 fonts
-- text extraction: properly support TrueType fonts

-- SPEC: Type3 fonts: consider case where font is in scope for content
-- stream that defines its glyph

-- SPEC: Type3 fonts: may refer to resrc dicts, which may loop
