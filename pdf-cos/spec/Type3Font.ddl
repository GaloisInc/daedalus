-- Type3Font: Type 3 fonts
import FontCommon
import CMap

def PartialType3Font (com : PartialFontCommon) (pChars : PartialCharSet)
  (pBBox : maybe Rectangle) (pMatrix : maybe FontMatrix)
  (pCharProcs : maybe [ [ uint 8 ] -> Stream ]) (pEnc : maybe EncodingDict) = {
  common = com
; chars = pChars
; fontBBox = pBBox
; fontMatrix = pMatrix
; charProcs = pCharProcs
; encoding = pEnc
}

def InitType3Font : PartialType3Font = {
  common = InitCommonFont
; chars = InitCharSet
; fontBBox = nothing
; fontMatrix = nothing
; charProcs = nothing
; encoding = nothing
}

def Type3SetCommon (coms : PartialFontCommon) (f : PartialType3Font) =
  PartialType3Font
    coms
    f.chars
    f.fontBBox
    f.fontMatrix
    f.charProcs
    f.encoding

def Type3SetChars (cs : PartialCharSet) (f : PartialType3Font) =
  PartialType3Font
    f.common
    cs
    f.fontBBox
    f.fontMatrix
    f.charProcs
    f.encoding

-- TODO: complete
def AddCharProcs (f : PartialType3Font) = {
}

def AddCharProcs (f : PartialType3Font) = {
}

def ExtendType3Font k font = {
  @cf0 = ExtendCommonFont "Type3" SimpleFontType
           k font.common;
  case cf0 of {
    just cf1 -> just (Type3SetCommon cf1 font)
  ; nothing -> case ExtendCharSet k font.chars of {
        just chars0 -> just (Type3SetChars chars0 font)
      ; nothing -> -- fields specific to Type3 fonts
          if k == "CharProcs" then {
            font.charProcs is nothing;
            just (AddCharProcs font)
          }
          else if k == "Encoding" then {
            font.encoding is nothing;
            just (AddEncoding font)
          }
          else if k == "Resoruces" then {
            font.resources is nothing;
            just (AddResources font)
          }
          else nothing
      }
  }
}
