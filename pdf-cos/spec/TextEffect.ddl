-- TextEffects: text state and effects
import Map
import Stdlib

import Encoding
import FontDict
import Glyph
import GlyphList
import StdEncoding
import Type0Font
import Type1Font
import Unicode

-- TODO: indirect deps
import CMap
import CIDFont
import PdfValue


def SizedFont (f : FontDict) (s: int) = {
  font = f;
  size = s;
}

def TextState (cs: int) (ws: int) (s: int) (l: Number)
  (rm: int) (r: int) = {
  charSpace = cs;
  wordSpace = ws;
  scale = s;
  leading = l;
  renderingMode = rm;
  rise = r;
}

-- InitTextState: the initial text state (Table 103)
def InitTextState : TextState = TextState
  0
  0
  100
  (IntNumber 0)
  0
  0

-- setter operations for text state:
def SetCharSpace (cs : int) (q : TextState) : TextState = TextState
  cs
  q.wordSpace
  q.scale
  q.leading
  q.renderingMode
  q.rise

def SetLeading (l : Number) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  l
  q.renderingMode
  q.rise

def SetRenderingMode (rm : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  q.leading
  rm
  q.rise

def SetRise (r : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  q.leading
  q.renderingMode
  r

def SetScale (s : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  s
  q.leading
  q.renderingMode
  q.rise

def SetWordSpace (ws : int) (q : TextState) : TextState = TextState
  q.charSpace
  ws
  q.scale
  q.leading
  q.renderingMode
  q.rise

def InitBytes : [ UTF8 ] = [ ]

def TextEffect (q : TextState) (bs : [ UTF8 ]) = {
  textState = q;
  output = bs;
}

def InitEffect = TextEffect InitTextState InitBytes

def LiftToTextEffect (q : TextState) = TextEffect q [ ]

def PutStr (eff: TextEffect) (bs : [ UTF8 ] ) = TextEffect
  eff.textState (append eff.output bs)

def Sequence (eff0: TextEffect) (eff1 : TextEffect) = TextEffect
  eff1.textState (append eff0.output eff1.output)

def SetEffectState (q : TextState) (eff: TextEffect) = Sequence
  (LiftToTextEffect q) eff

def UTF81Code = {
  Many NonASCIIByte; -- eat non-ASCII bytes
  UnicodeASCII
}

-- ExtractString in reference to Section 9.10.2 in the spec
--
--  - not 100% sure that doing step 1 ("if the font dictionary contains a
--    ToUnicode CMap ...") really works for a Type0 font.
--
--  - FIXME: We can revisit once we get into the details of Type0/Composite fonts.
--
--  - FIXME: looking at CMap.FontCode: maybe step 1 would work for Type0 font?

def ExtractString (q: TextState) (szFont: SizedFont) (s : [ uint 8 ]) :
  [ UTF8 ] = 
  -- assume that there is no "default font" if not set by "Tf"
  case szFont.font of {
    type0 font0 -> ExtractString_Composite font0 q s
  ; type1 font1 -> ExtractStringType1 font1 s
  ; mmfont fontMM -> ExtractStringType1 fontMM s
  ; type3 font3 -> ExtractStringType3 font3 s
  }

def ExtractString_Composite (f: Type0Font) (q: TextState) (s : [ uint 8 ]) :
  [ UTF8 ] = {
  -- TODO: use f to encode character codes in UTF
  WithStream (arrayStream s) (Many UTF81Code)
}

-- ExtractStringType1: extract string under a Type1 font
def ExtractStringType1 (font : Type1Font) (s : [ uint 8 ]) : [ UTF8 ] = {
  -- TODO: use ToUnicode field, if defined

  -- compose [code -> glyph] and [glyph -> unicode] maps
  @code2Uni = ComposeMaps
    -- build [ code -> glyph ] by inspecting the font
    (case font.encoding of {
      just enc -> case enc of {
        predefEnc preDef -> -- use a pre-defined encoding
          PredefEncoding preDef
      ; encDict enc0 -> {
          -- use differences applied to pre-defined encoding
          MapUnion
            (case enc0.baseEncoding of {
              just baseEnc -> PredefEncoding baseEnc
            ; nothing -> StdEncoding;
            })
            enc0.differences
        } 
      }
    ; nothing -> StdEncoding -- use the standard encoding
    })
    -- TODO: GlyphMap may be very expensive; memoize?
    GlyphMap;

  -- parse the string, looking up each code in the unicode map
  WithStream (arrayStream s) (concat (Many (Lookup UInt8 code2Uni)))
}

-- ExtractStringType1: extract string under a Type1 font
-- TODO: call this to extract text from Type3 fonts
def ExtractStringType3 (font : Type3Font) (s : [ uint 8 ]) : [ UTF8 ] = {
  -- TODO: use ToUnicode, if defined

  -- TODO: GlyphMap may be very expensive; memoize?
  -- compose [code -> glyph] and [glyph -> unicode] maps
  @code2Uni = ComposeMaps
    -- for Type3 fonts, encoding is required and Differences entirely
    -- specifies the character encoding
    font.encoding.differences
    GlyphMap;

  -- parse the string, looking up each code in the unicode map
  WithStream (arrayStream s) (concat (Many (Lookup UInt8 code2Uni)))
}

-- TODO: use q.charSpace to inject space when necessary

-- TODO: use q.wordSpace to remove space when necessary

-- TODO: Table 104: Text rendering modes: neither fill nor stroke is
-- treated as invisible: should we replace with whitespace?
