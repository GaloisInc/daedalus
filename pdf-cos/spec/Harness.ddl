-- Harness: testing harness
import Stdlib

import ResourceDict
import TextEffect

import Encoding
import FontCommon
import FontDict
import GenPdfValue
-- import Page
import PageTreeNode
import PdfValue
import Type0Font
import Type1Font
import Type3Font
import FontDesc
import TextObj
import ContentStreamLight

-- def TestResrcs : ResourceDict = ResourceDict (PartialResourceDict
--   nothing
--   nothing
--   nothing
--   nothing
--   nothing
--   (just
--     (Insert "F16" TestFont
--       (Insert "F2" TestFont
--         (Insert "F3" TestFont
--           (Insert "F4" TestFont
--             empty)))))
--   nothing
--   nothing)

def TestRef : Ref = {
  obj = 0;
  gen = 0;
}

-- Main: the entry point
def Main = EncodingP

-- TODO:

-- text extraction: properly support Type0 fonts
-- text extraction: properly support TrueType fonts

-- DDL: huge build times for dict list

-- DDL: compiler allows xor over ints, interpreter doesn't

-- SPEC: Type3 fonts: consider case where font is in scope for content
-- stream that defines its glyph

-- SPEC: Type3 fonts: may refer to resrc dicts, which may loop
