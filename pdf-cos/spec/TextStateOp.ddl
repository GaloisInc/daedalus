-- TextStateOp: operations that only update text state
import Stdlib

import PdfValue
import GenPdfValue
import TextEffect

-- Text state operators (Table 103)
def TextStateOp resrcs = Choose1 { -- operations are mutually exclusive
  setCharSpace = {
    $$ = Token Integer;
    KW "Tc"
  };
  setWordSpace = {
    $$ = Token Integer;
    KW "Tw"
  };
  setScale = {
    $$ = Token Integer;
    KW "Tz"
  };
  setLeading = {
    $$ = Token Integer;
    KW "TL"
  };
  setFont = {
    @fontNm = Token Name;
    @font = Lookup fontNm resrcs.font;
    @size = Token Integer;
    KW "Tf";
    SizedFont font size
  };
  setRenderingMode = {
    $$ = Token Integer; 
    KW "Tr"
  };
  setRise = {
    $$ = Token Integer;
    KW "Ts"
  }
}

def SetCharSpaceOp (x : int) : TextStateOp = {| setCharSpace = ^x |}

def SetLeadingOp (y : int) : TextStateOp = {| setLeading = ^y |}

def SetWordSpaceOp (x : int) : TextStateOp = {| setWordSpace = ^x |}

-- UpdTextState op q: state q updated by running text-state operation op
def UpdTextState (op: TextStateOp) (q: TextState) : TextState = case op of {
  setCharSpace charSpace -> SetCharSpace charSpace q
; setWordSpace wordSpace -> SetWordSpace wordSpace q
; setScale s -> SetScale s q
; setLeading l -> SetLeading l q
; setFont sf -> SetSizedFont sf q
; setRenderingMode rm -> SetRenderingMode rm q
; setRise rs -> SetRise rs q
}

