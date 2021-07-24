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

def WithFont P (resrcs : ResourceDict) (szFont : maybe SizedFont) = Sum1
  { @fontNm = Token Name; -- parse operation that sets font
    @font = Lookup fontNm resrcs.font; -- lookup the name in the font dict
    @size = Token Integer;
    KW "Tf";
    just (SizedFont font size)
  }
  (P szFont)

def OpsWithFont P resrcs szFont0 = ManyWithState (WithFont P resrcs) szFont0

-- TODO: refator this into a loop for maintaining state and a font state

-- TODO: weaken this to ignore ops that it doesn't recognize

-- TextOp: a text operation
def TextOp (mayF : maybe SizedFont) = Choose1 {
  -- all text operands are mutually exclusive
  textShowOp = TextShowOp (mayF is just);
  textStateOp = TextStateOp;
  textPosOp = TextPosOp;
  macroOp = MacroOp;
}

def MvNextLineStart : MacroOp = {| mvNextLineStart = { } |}

def MvNextLineWOffset (tx : int) (ty : int) : TextPosOp = {|
  setTextMatrix = SetMatrixOp 
    0
    0
    0
    0
    tx
    ty
|}

def MvNextLineShow (f : SizedFont) (s : string) : [ TextOp ] = [
  {| macroOp = MvNextLineStart |}
, {| textShowOp = ShowStringOp f s |}
]

-- TextOpP: parses a text opcode into a sequence of text operations
def TextOpP (f : maybe SizedFont) : [ TextOp ] = Choose1 {
  -- parse an actual text operation:
  [ TextOp f ];
  -- text operations that are basically macros over other
  -- operations. TD operations: (Table 106)
  { @tx = Token Integer;
    @ty = Token Integer;
    KW "Td"; -- Table 106
    [ {| textPosOp = MvNextLineWOffset tx ty |} ]
  };
  { @tx = Token Integer;
    @ty = Token Integer;
    KW "TD"; -- Table 106
    [ {| textStateOp = SetLeadingOp ty |}
    , {| textPosOp = MvNextLineWOffset tx ty |}
    ]
  };
  { @s = Token String;
    KW "'"; -- Table 107
    MvNextLineShow (f is just) s
  };
  { @aw = Token Integer;
    @ac = Token Integer;
    @s = Token String;
    KW "\""; -- Table 107
    append 
      [ {| textStateOp = SetWordSpaceOp aw |}
      , {| textStateOp = SetCharSpaceOp ac |}
      ]
      (MvNextLineShow (f is just) s)
  };
  -- eat the other operators that are allowed in a text object
  (When GraphicsStateOp [ ]);
  (When ColourOp [ ]);
  (When MarkedContentOp [ ]);
}

-- TextObj: a text object
def TextObj (rd: ResourceDict) (f : maybe SizedFont) : [ TextOp ] =
  Between "BT" "ET" (concat (OpsWithFont TextOpP rd f))

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
--    ; macroOp -> ... TODO: implement 
    }
  }
}
