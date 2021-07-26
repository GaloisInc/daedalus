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

-- text extraction: support TrueType fonts
-- text extraction: support Type0 fonts

-- TODO: test
-- pages
-- page tree nodes

-- SPEC: Type3 fonts: consider case where font is in scope for content
-- stream that defines its glyph

-- TODO: include in .cabal
        -- ColourSpaceOps
        -- Encoding
        -- FontCommon
        -- Glyph
        -- GlyphList
        -- GraphicsStateOps
        -- MacEncoding
        -- MarkedContentOps
        -- Maybe
        -- Rectangle
        -- StdEncoding
        -- Type3Font
        -- WinEncoding
