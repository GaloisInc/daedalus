-- Harness: testing harness
import ResourceDict
import TextEffect
import FontDict
import Type0Font
import Type1Font
import FontDesc
import TextObj
import ContentStreamLight

def TestFont : FontDict = MkType0Font (Type0Font (PartialType0Font
  true
  true
  (just Helvetica)
  (just (PreDefEncoding "TestEnc"))
  nothing
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
def Main = ContentStreamP TestResrcs

-- TODO: test
-- text extraction
-- pages
-- page tree nodes

-- Type3 fonts: consider case where font is in scope for content
-- stream that defines its glyph
