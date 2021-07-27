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

def CommonFontWitness = PartialCommonFont true true nothing

def Test0Font : FontDict = MkType0Font (Type0Font (PartialType0Font
  CommonFontWitness
  (just Helvetica)
  (just (PreDefEncoding "TestEnc"))
  nothing))

def Test1Font : FontDict = MkType1Font (Type1Font (PartialType1Font
  CommonFontWitness
  InitCharSet
  (just Helvetica)
  nothing))

def TestResrcs : ResourceDict = ResourceDict (PartialResourceDict
  nothing
  nothing
  nothing
  nothing
  nothing
  (just (Insert "F13" Test1Font empty))
  nothing
  nothing)

def TestSizedFont = SizedFont Test1Font 12

-- Main: the entry point
def Main = ExtractContentStreamText (ContentStreamP TestResrcs)

-- TODO:

-- TrueType

-- regression tset content streams
-- pages
-- page tree nodes
-- whole PDFs

-- text extraction: properly support Type0 fonts
-- text extraction: properly support TrueType fonts

-- DDL: huge build times for dict list

-- SPEC: Type3 fonts: consider case where font is in scope for content
-- stream that defines its glyph

-- SPEC: Type3 fonts: may refer to resrc dicts, which may loop
