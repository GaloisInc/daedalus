-- TextShowOp: text showing operators
import Stdlib

import PdfValue
import GenPdfValue
import TextEffect

def TJOper = Choose {
  shownString = String;
  adjustNum = Number;
}

-- Text-showing operators (Table 107)
def TextShowOp = Choose1 { -- operations are mutually exclusive:
  showString = {
    $$ = Token String;
    KW "Tj" 
  };
  showManyStrings = {
    $$ = GenArray TJOper;
    KW "TJ"
  };
  setTextMatrix = { -- updates text matrix, which affects user space
    a = Token Integer;
    b = Token Integer;
    c = Token Integer;
    d = Token Integer;
    e = Token Integer;
    f = Token Integer;
    KW "Tm";
  };
}

def SetMatrixOp (pa: int) (pb: int) (pc: int)
  (pd: int) (pe: int) (pf: int) : TextShowOp = {|
  setTextMatrix = {
    a = pa;
    b = pb;
    c = pc;
    d = pd;
    e = pe;
    f = pf;
  }
|}

def ShowStringOp (s : string) : TextShowOp = {| showString = s |}

-- Text-showing operators: Table 107
def UpdTextShow (op: TextShowOp) (q : TextState) : [ UTF8 ] = case op of {
  showString s -> ExtractString q s
; showManyStrings args ->
  for (acc = []; a in args) {
    append acc (case (a : TJOper) of {
      shownString s -> ExtractString q s
    ; adjustNum -> [ ] -- TODO: possibly refine 
    })
  }
; setTextMatrix _ -> [ ]
}
