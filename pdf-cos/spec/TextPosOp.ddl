-- TextPosOp: text positioning operators
import Stdlib
import GenPdfValue
import PdfValue

import TextEffect
import Unicode


def SetMatrixOp (pa: int) (pb: int) (pc: int)
  (pd: int) (pe: int) (pf: int) = {
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
      (Token Integer)
      (Token Integer)
      (Token Integer)
      (Token Integer)
      (Token Integer)
      (Token Integer);
    KW "Tm";
  };
}

def ShowPos (op: TextPosOp) : [ UTF8 ] = case op of {
  -- interpret setting text position as injecting a newline
  setTextMatrix m -> [ UTF8Ascii '\n' ]
}
