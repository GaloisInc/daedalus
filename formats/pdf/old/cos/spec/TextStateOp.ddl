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
    $$ = Token Number;
    KW "Tc"
  };
  setWordSpace = {
    $$ = Token Number;
    KW "Tw"
  };
  setScale = {
    $$ = Token Number;
    KW "Tz"
  };
  setLeading = {
    $$ = Token Number;
    KW "TL"
  };
  setRenderingMode = {
    $$ = Token Number; 
    KW "Tr"
  };
  setRise = {
    $$ = Token Number;
    KW "Ts"
  }
}

def setCharSpaceOp (x : Number) : TextStateOp = {| setCharSpace = x |}

def setLeadingOp (y : Number) : TextStateOp = {| setLeading = y |}

def setWordSpaceOp (x : Number) : TextStateOp = {| setWordSpace = x |}

-- updTextState op q: state q updated by running text-state operation op
def updTextState (op: TextStateOp) (q: textState) : textState = case op of {
  setCharSpace charSpace -> setCharSpace charSpace q
; setWordSpace wordSpace -> setWordSpace wordSpace q
; setScale s -> setScale s q
; setLeading l -> setLeading l q
; setRenderingMode rm -> setRenderingMode rm q
; setRise rs -> setRise rs q
}

