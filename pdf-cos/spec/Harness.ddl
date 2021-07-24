-- Harness: testing harness
import ResourceDict
-- import Type1Font
import TextEffect
import FontDict
import Type0Font
import FontDesc
import TextObj

def TestFont : FontDict = MkType0Font (Type0Font (MkPartialType0Font
    true
    true
    (just "TestFont")
    (just {| preDef = "TestEnc"|})
    nothing
    nothing))

def Map1 k v = Insert k v empty

def Main = TextObj
  (ResourceDict InitResourceDict)
  (just (SizedFont TestFont 12))

-- TODO: unit test these parsers
-- content stream
-- pages
-- page tree nodes

-- embedded font programs
-- Type 2 CIDFonts
-- Compact Font Format

-- unknown CIDFontType

-- CIDKey
