import Stdlib
import Array

import GenPdfValue
import PdfValue
import ResourceDict

import TextEffect
import TextShowOp
import TextStateOp

-- TODO: non-immediate dep
import FontDict
import Unicode

def MacroOp = Choose1 {
  mvNextLineStart = KW "T*";
}

-- TextOp: a text operation
def TextOp (rd: ResourceDict) = Choose1 {
  -- all text operands are mutually exclusive
  textShowOp = TextShowOp;
  textStateOp = TextStateOp rd;
  macroOp = MacroOp;
}

def MvNextLineStart : MacroOp = {|
  mvNextLineStart = { }
|}

def MvNextLineWOffset (tx : int) (ty : int) : TextShowOp = SetMatrixOp 
  0
  0
  0
  0
  tx
  ty

def MvNextLineShow (s : string) : [ TextOp ] = [
  {| macroOp = MvNextLineStart |}
, {| textShowOp = ShowStringOp s |}
]

-- TextOpP: a parser for a sequence of text operations
def TextOpP (rd: ResourceDict) : [ TextOp ] = (LiftPToArray (TextOp rd)) <|
  -- text operations that are basically macros over other
  -- operations. TD operations: (Table 106)
  { @tx = Token Integer;
    @ty = Token Integer;
    KW "Td"; -- Table 106
    [ {| textShowOp = MvNextLineWOffset tx ty |} ]
  } <|
  { @tx = Token Integer;
    @ty = Token Integer;
    KW "TD"; -- Table 106
    [ {| textStateOp = SetLeadingOp ty |}
    , {| textShowOp = MvNextLineWOffset tx ty |}
    ]
  } <|
  { @s = Token String;
    KW "'"; -- Table 107
    MvNextLineShow s
  } <|
  { @aw = Token Integer;
    @ac = Token Integer;
    @s = Token String;
    KW "\""; -- Table 107
    append 
      [ {| textStateOp = SetWordSpaceOp aw |}
      , {| textStateOp = SetCharSpaceOp ac |}
      ]
      (MvNextLineShow s)
  }

-- TextObj: a text object
def TextObj (rd: ResourceDict) : [ TextOp ] = Between "BT" "ET"
  (concat (Many (TextOpP rd)))

def InterpTextObj (obj: [ CTextOp ]) (q : TextState) : TextEffect = {
  @eff0 = LiftToTextEffect q;
  for (effAcc = eff0; op in obj) {
    case (op : TextOp) of {
      textShowOp showOp -> PutStr effAcc
        (UpdTextShow showOp effAcc.textState)
    ; textStateOp stateOp -> TextEffect 
        (UpdTextState stateOp effAcc.textState)
        effAcc.output
--    ; macroOp -> ... TODO
    }
  }
}
