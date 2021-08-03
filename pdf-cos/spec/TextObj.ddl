import Stdlib
import Sum
import Array
import Pair

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
import Encoding
import FontDict
import Type1Font
import Type3Font
import Unicode

def MacroOp = Choose1 {
  mvNextLineStart = KW "T*";
}

def FontEffect (szFont : maybe sizedFont) x = {
  fst = szFont;
  snd = x;
}

-- GenSum1: biased sum over parameterized parsers
def GenSum1 P0 P1 x =
  (P0 x) <|
  (P1 x)

-- LiftRes...: lift a result parser to a font effect parser
def LiftResToFontEffect P (szFont : maybe sizedFont) = FontEffect
  szFont
  (just (P szFont))

-- FontOp: parse a sized font 
def FontOp (resrcs : ResourceDict) = {
  @fontNm = Token Name;
  @size = Token Number; -- parse font size
  KW "Tf";
  sizedFont (Lookup fontNm resrcs.font) size
}

-- FontOpEffect: font ops, lifted into an effect
def FontOpEffect (resrcs : ResourceDict) (szFont : maybe sizedFont) = FontEffect
  (just (FontOp resrcs))
  nothing

def TestsizedFont = sizedFont (MkType1Font Type1FontStub) (intNumber 12)

-- TextOp: an operation that can occur in a text object
def TextOp (mayF : maybe sizedFont) = Choose1 {
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

def MvNextLineWOffset (tx : Number) (ty : Number) : TextPosOp = {|
  setTextMatrix = setMatrixOp 
    (intNumber 0) (intNumber 0)
    (intNumber 0) (intNumber 0)
    tx ty
|}

def MvNextLineShow (f : sizedFont) (s : [uint 8]) : [ TextOp ] = [
  {| macroOp = MvNextLineStart |}
, {| textShowOp = ShowStringOp f s |}
]

-- TextOpP: parses a text opcode into a sequence of text operations
def TextOpP (f : maybe sizedFont) : [ TextOp ] = Choose1 {
  -- parse an actual text operation:
  [ TextOp f ]
  -- text operations that are basically macros over other
  -- operations. TD operations: (Table 106)
; { @tx = Token Number;
    @ty = Token Number;
    KW "Td"; -- Table 106
    [ {| textPosOp = MvNextLineWOffset tx ty |} ]
  }
; { @tx = Token Number;
    @ty = Token Number;
    KW "TD"; -- Table 106
    [ {| textStateOp = setLeadingOp ty |}
    , {| textPosOp = MvNextLineWOffset tx ty |}
    ]
  }
; { @s = Token String;
    KW "'"; -- Table 107
    MvNextLineShow (f is just) s
  }
; { @aw = Token Number;
    @ac = Token Number;
    @s = Token String;
    KW "\""; -- Table 107
    append 
      [ {| textStateOp = setWordSpaceOp aw |}
      , {| textStateOp = setCharSpaceOp ac |}
      ]
      (MvNextLineShow (f is just) s)
  }
}

-- TextObj: a text object
def TextObj (rd: ResourceDict) (f : maybe sizedFont) : FontEffect = 
  Between "BT" "ET" {
    @sf = ManyWithState
      (GenSum1
        (FontOpEffect rd)
        (LiftResToFontEffect TextOpP) )
      f;
    -- flatten out potential macro expansions of operators
    FontEffect sf.fst (concat sf.snd)
  }

def InterpTextObj (obj: [ TextOp ]) (q : textState) : textEffect = {
  @eff0 = liftToTextEffect q;
  for (effAcc = eff0; op in obj) {
    case (op : TextOp) of {
      textShowOp showOp -> putStr
        effAcc
        (ShowTextShow showOp effAcc.textState)
    ; textStateOp stateOp -> setEffectState
        (updTextState stateOp effAcc.textState)
        effAcc
    ; textPosOp posOp -> putStr
        effAcc
        (ShowPos posOp)
    ; _ -> effAcc
--    ; macroOp -> effAcc TODO:
    }
  }
}

-- TODO: fix to return font
