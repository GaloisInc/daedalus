-- TextEffects: text state and effects
import Stdlib

import FontDict
import Unicode
import Type0Font

def SizedFont (f : FontDict) (s: int) = {
  font = f;
  size = s;
}

def TextState (cs: int) (ws: int) (s: int) (l: int)
  (sf: maybe SizedFont) (rm: int) (r: int) = {
  charSpace = cs;
  wordSpace = ws;
  scale = s;
  leading = l;
  sizedFont = sf;
  renderingMode = rm;
  rise = r;
}

-- InitTextState: the initial text state (Table 103)
def InitTextState : TextState = TextState
  0
  0
  100
  0
  nothing
  0
  0

-- setter operations for text state:
def SetCharSpace (cs : int) (q : TextState) : TextState = TextState
  cs
  q.wordSpace
  q.scale
  q.leading
  q.sizedFont
  q.renderingMode
  q.rise

def SetSizedFont (f : SizedFont) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  q.leading
  (just f)
  q.renderingMode
  q.rise

def SetLeading (l : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  l
  q.sizedFont
  q.renderingMode
  q.rise

def SetRenderingMode (rm : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  q.leading
  q.sizedFont
  rm
  q.rise

def SetRise (r : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  q.scale
  q.leading
  q.sizedFont
  q.renderingMode
  r

def SetScale (s : int) (q : TextState) : TextState = TextState
  q.charSpace
  q.wordSpace
  s
  q.leading
  q.sizedFont
  q.renderingMode
  q.rise

def SetWordSpace (ws : int) (q : TextState) : TextState = TextState
  q.charSpace
  ws
  q.scale
  q.leading
  q.sizedFont
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
  UnicodeByte
}

-- See 9.10.2 in the spec, not 100% sure that doing step 1 ("if the font
-- dictinoary contains a ToUnicode CMap ...") really works for a Type0 font.  We
-- can revisit once we get into the details of Type0/Composite fonts.

def ExtractString (q: TextState) (s : [ uint 8 ]) : [ UTF8 ] = {
  @szFont = q.sizedFont is just;
    -- assume that there is no "default font" if not set by "Tf"
  case szFont.font of
    type0 fontdict  -> ExtractString_Composite fontdict    q s
    _               -> ExtractString_Simple    szFont.font q s
}

def ExtractString_Composite (f: Font_Type0) (q: TextState) (s : [ uint 8 ]) : [ UTF8 ] = {
  WithStream (arrayStream s) (Many UTF81Code)
  -- TODO: define: use the font in q to encode s in UTF8
}

def ExtractString_Simple    (f: Font_Dict ) (q: TextState) (s : [ uint 8 ]) : [ UTF8 ] = {
  WithStream (arrayStream s) (Many UTF81Code)
  -- TODO: define: use the font in q to encode s in UTF8
}

-- TODO: use q.charSpace to inject space when necessary

-- TODO: use q.wordSpace to remove space when necessary

-- TODO: Table 104: Text rendering modes: neither fill nor stroke is
-- treated as invisible: should we replace with whitespace?

-- PDFA: should we model the text matrix and operations that set it
-- directly (Td/TD/Tm)? We would need to agree on expected behavior for:
-- * instances of Td/TD/Tm that set tx to something negative wo setting ty to next line
-- * instances of Td/TD/Tm that set ty to something less than leading;

-- leading:
-- * T* that when value of leading is very small/negative?

-- Note that there's an alternative here where we use the text state other than the text matrix (Table 103): just char space, word space (and maybe leading).
