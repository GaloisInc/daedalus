-- TextPosOp: text positioning operators
import Stdlib
import GenPdfValue
import PdfValue

import TextEffect
import Unicode


def SetMatrixOp (pa: Number) (pb: Number)
  (pc: Number) (pd: Number)
  (pe: Number) (pf: Number) = {
  a = pa;
  b = pb;
  c = pc;
  d = pd;
  e = pe;
  f = pf;
}

-- Text-positioning operators (Table 106)
def TextPosOp = Choose1 { 
  setTextMatrix = {
    $$ = SetMatrixOp
      (Token Number)
      (Token Number)
      (Token Number)
      (Token Number)
      (Token Number)
      (Token Number);
    KW "Tm";
  };
}

def ShowPos (op: TextPosOp) : [ UTF8 ] = case op of {
  -- interpret setting text position as injecting a newline
  setTextMatrix m -> [ UTF8Ascii '\n' ]
}
