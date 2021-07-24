-- Harness: testing harness
import ResourceDict
import TextEffect
import FontDict
import Type0Font
import Type1Font
import FontDesc
import TextObj

def TestFont : FontDict = MkType0Font (Type0Font (PartialType0Font
  true
  true
  (just Helvetica)
  (just {| preDef = "TestEnc"|})
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

def Main = ResourceDictP

-- TODO: unit test these parsers
-- other examples in Sec. 9.2.2
-- text objects
-- content stream
-- pages
-- page tree nodes

-- embedded font programs
-- Type 2 CIDFonts
-- Compact Font Format

-- unknown CIDFontType

-- CIDKey
