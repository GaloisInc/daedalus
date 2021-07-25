-- TextStateOp: operations that only update text state
import Stdlib

import PdfValue
import GenPdfValue
import TextEffect

-- TODO: requires indirect import
import FontDict

-- Text state operators (Table 103)
def TextStateOp = Choose1 { -- operations are mutually exclusive
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
    $$ = Token Number;
    KW "TL"
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

def SetLeadingOp (y : Number) : TextStateOp = {| setLeading = y |}

def SetWordSpaceOp (x : int) : TextStateOp = {| setWordSpace = ^x |}

-- UpdTextState op q: state q updated by running text-state operation op
def UpdTextState (op: TextStateOp) (q: TextState) : TextState = case op of {
  setCharSpace charSpace -> SetCharSpace charSpace q
; setWordSpace wordSpace -> SetWordSpace wordSpace q
; setScale s -> SetScale s q
; setLeading l -> SetLeading l q
; setRenderingMode rm -> SetRenderingMode rm q
; setRise rs -> SetRise rs q
}

