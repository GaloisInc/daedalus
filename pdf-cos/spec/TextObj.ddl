import Stdlib
import Sum
import Array

import GenPdfValue
import PdfValue
import ResourceDict

import ColourSpaceOps
import GraphicsStateOps
import MarkedContentOps

import TextEffect
import TextPosOp
import TextShowOp
import TextStateOp

-- TODO: non-immediate dep
import FontDict
import Unicode

def MacroOp = Choose1 {
  mvNextLineStart = KW "T*";
}

def FontEffect (szFont : maybe SizedFont) x : Pair = {
  fst = szFont;
  snd = x;
}

-- GenSum1: sum over parameterized parsers
def GenSum1 P0 P1 x =
  (P0 x) <|
  (P1 x)

-- LiftRes...: lift a result parser to a font effect parser
def LiftResToFontEffect P (szFont : maybe SizedFont) = FontEffect
  szFont
  (just (P szFont))

-- FontOp: parse a sized font 
def FontOp (resrcs : ResourceDict) = {
  @fontNm = Token Name;
  @size = Token Integer; -- parse font size
  KW "Tf";
  SizedFont (Lookup fontNm resrcs.font) size
}

-- FontOpEffect: font ops, lifted into an effect
def FontOpEffect (resrcs : ResourceDict) (szFont : maybe SizedFont) = FontEffect
  (just (FontOp resrcs))
  nothing

-- TextOp: an operation that can occur in a text object
def TextOp (mayF : maybe SizedFont) = Choose1 {
  -- all text operands are mutually exclusive
  textShowOp = TextShowOp (mayF is just);
  textStateOp = TextStateOp;
  textPosOp = TextPosOp;
  textGraphicsStateOp = GraphicsStateOp;
  textColourOp = ColourOp;
  textMarkedPoint = MarkContentPoint;
  textMarkedSeq = MarkedContentSeqOp;
  -- TODO: refine to parse well-nested sequences
  macroOp = MacroOp;
}

def MvNextLineStart : MacroOp = {| mvNextLineStart = { } |}

def MvNextLineWOffset (tx : int) (ty : int) : TextPosOp = {|
  setTextMatrix = SetMatrixOp 
    0 0
    0 0
    tx ty
|}

def MvNextLineShow (f : SizedFont) (s : string) : [ TextOp ] = [
  {| macroOp = MvNextLineStart |}
, {| textShowOp = ShowStringOp f s |}
]

-- TextOpP: parses a text opcode into a sequence of text operations
def TextOpP (f : maybe SizedFont) : [ TextOp ] = Choose1 {
  -- parse an actual text operation:
  [ TextOp f ]
  -- text operations that are basically macros over other
  -- operations. TD operations: (Table 106)
; { @tx = Token Integer;
    @ty = Token Integer;
    KW "Td"; -- Table 106
    [ {| textPosOp = MvNextLineWOffset tx ty |} ]
  }
; { @tx = Token Integer;
    @ty = Token Integer;
    KW "TD"; -- Table 106
    [ {| textStateOp = SetLeadingOp ty |}
    , {| textPosOp = MvNextLineWOffset tx ty |}
    ]
  }
; { @s = Token String;
    KW "'"; -- Table 107
    MvNextLineShow (f is just) s
  }
; { @aw = Token Integer;
    @ac = Token Integer;
    @s = Token String;
    KW "\""; -- Table 107
    append 
      [ {| textStateOp = SetWordSpaceOp aw |}
      , {| textStateOp = SetCharSpaceOp ac |}
      ]
      (MvNextLineShow (f is just) s)
  }
}

-- TextObj: a text object
def TextObj (rd: ResourceDict) (f : maybe SizedFont) : FontEffect = 
  Between "BT" "ET" {
    @sf = ManyWithState
      (GenSum1
        (FontOpEffect rd)
        (LiftResToFontEffect TextOpP) )
      f;
    -- flatten out potential macro expansions of operators
    FontEffect sf.fst (concat sf.snd)
  }

def InterpTextObj (obj: [ TextOp ]) (q : TextState) : TextEffect = {
  @eff0 = LiftToTextEffect q;
  for (effAcc = eff0; op in obj) {
    case (op : TextOp) of {
      textShowOp showOp -> PutStr
        effAcc
        (ShowTextShow showOp effAcc.textState)
    ; textStateOp stateOp -> SetEffectState
        (UpdTextState stateOp effAcc.textState)
        effAcc
    ; textPosOp posOp -> PutStr
        effAcc
        (ShowPos posOp)
    ; _ -> effAcc
--    ; macroOp -> ... TODO: define
    }
  }
}

-- TODO: fix to return font
