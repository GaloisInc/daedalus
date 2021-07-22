import Stdlib
import Array

import GenPdfValue
import PdfValue
import ResourceDict

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

-- ParseWFont: parse a sequence of operators, maintaining font
def ParseWFont resrcs szFont0 P = Default [ ] {
  Choose1 {
    { @fontNm = Token Name; -- parse operation that sets font
      @font = Lookup fontNm resrcs.font;
      @size = Token Integer;
      KW "Tf";
      ParseWFont resrcs (just (SizedFont font size)) P
    };
    (cons (P szFont0) (ParseWFont resrcs szFont0 P))
  }
}

-- TextOp: a text operation
def TextOp (rd: ResourceDict) (mayF : maybe SizedFont) = Choose1 {
  -- all text operands are mutually exclusive
  textShowOp = TextShowOp (mayF is just);
  textStateOp = TextStateOp rd;
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

-- TextOpP: a parser for a sequence of text operations
def TextOpP (rd: ResourceDict) (f : maybe SizedFont) : [ TextOp ] = Choose1 {
  [ TextOp rd f ];
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
}

-- TextObj: a text object
def TextObj (rd: ResourceDict) (f : maybe SizedFont) : [ TextOp ] =
  Between "BT" "ET" (concat (ParseWFont rd f (TextOpP rd)))

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
