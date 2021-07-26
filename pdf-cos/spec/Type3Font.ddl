-- Type3Font: Type 3 fonts
import Rectangle
import GenPdfValue
import PdfValue
import PdfDecl

import Encoding
import FontDesc
import FontCommon
import CMap

def FontMatrix = {
  a = Token Number;
  b = Token Number;
  c = Token Number;
  d = Token Number;
  e = Token Number;
  f = Token Number;
}

def PartialType3Font (com : PartialCommonFont) (pChars : PartialCharSet)
  (pBBox : maybe Rectangle) (pMatrix : maybe FontMatrix)
  (pCharProcs : maybe [ [ uint 8 ] -> Ref ]) (pEnc : maybe EncodingDict)
  (pRes : maybe [ [ uint 8 ] -> Value ]) = {
  common = com
; chars = pChars
; fontBBox = pBBox
; fontMatrix = pMatrix
; charProcs = pCharProcs
; encoding = pEnc
; resources = pRes
}

def InitType3Font : PartialType3Font = {
  common = InitCommonFont
; chars = InitCharSet
; fontBBox = nothing
; fontMatrix = nothing
; charProcs = nothing
; encoding = nothing
; resources = nothing
}

def Type3SetCommon (coms : PartialCommonFont) (f : PartialType3Font) =
  PartialType3Font
    coms
    f.chars
    f.fontBBox
    f.fontMatrix
    f.charProcs
    f.encoding
    f.resources

def Type3SetChars (cs : PartialCharSet) (f : PartialType3Font) =
  PartialType3Font
    f.common
    cs
    f.fontBBox
    f.fontMatrix
    f.charProcs
    f.encoding
    f.resources

def AddFontBBox (f : PartialType3Font) = PartialType3Font
  f.common
  f.chars
  (just (DirectOrRef Rectangle))
  f.fontMatrix
  f.charProcs
  f.encoding
  f.resources

def AddFontMatrix (f : PartialType3Font) = PartialType3Font
  f.common
  f.chars
  f.fontBBox
  (just (DirectOrRef (Between "[" "]" FontMatrix)))
  f.charProcs
  f.encoding
  f.resources

def AddCharProcs (f : PartialType3Font) = PartialType3Font
  f.common
  f.chars
  f.fontBBox
  f.fontMatrix
  (just (DirectOrRef (PdfDict Ref)))
  -- TODO: parse this more precisely by parsing ref as content stream
  f.encoding
  f.resources

def Type3AddEncoding (f : PartialType3Font) = PartialType3Font
  f.common
  f.chars
  f.fontBBox
  f.fontMatrix
  f.charProcs
  (just (DirectOrRef EncodingP))
  f.resources

def AddResources (f : PartialType3Font) = PartialType3Font
  f.common
  f.chars
  f.fontBBox
  f.fontMatrix
  f.charProcs
  f.encoding
  (just (DirectOrRef Dict))
  -- TODO: parse this precisely by defining recursively with ResourceDict

def ExtendType3Font k font = {
  @cf0 = ExtendCommonFont "Type3" SimpleFontType
           k font.common;
  case cf0 of {
    just cf1 -> just (Type3SetCommon cf1 font)
  ; nothing -> case ExtendCharSet k font.chars of {
        just chars0 -> just (Type3SetChars chars0 font)
      ; nothing -> -- fields specific to Type3 fonts
          if k == "FontBBox" then {
            font.fontBBox is nothing;
            just (AddFontBBox font)
          }
          else if k == "FontMatrix" then {
            font.fontMatrix is nothing;
            just (AddFontMatrix font)
          }
          else if k == "CharProcs" then {
            font.charProcs is nothing;
            just (AddCharProcs font)
          }
          else if k == "Encoding" then {
            font.encoding is nothing;
            just (Type3AddEncoding font)
          }
          else if k == "Resources" then {
            font.resources is nothing;
            just (AddResources font)
          }
          else nothing
      }
  }
}

def Type3Font (f : PartialType3Font) = {
  toUnicode = CommonFont f.common;
  charSet = CharSet (NonStandardFont "") f.chars; -- PDFA: what should this be?

  fontBBox = f.fontBBox is just;
  fontMatrix = f.fontMatrix is just;
  charProcs = f.charProcs is just;
  encoding = f.encoding is just;
  resources = f.resources is just;
}

def Type3FontP = GenPdfDict1
  InitType3Font
  ExtendType3Font
  Type3Font
