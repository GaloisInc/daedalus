-- TextShowOp: text showing operators
import Stdlib

import PdfValue
import GenPdfValue
import TextEffect

-- TODO: non-immediate dep
import Unicode

def TJOper = Choose {
  shownString = String;
  adjustNum = Number;
}

def TextShowOper = Choose1 { -- operations are mutually exclusive:
  showString = {
    $$ = Token String;
    KW "Tj" 
  };
  showManyStrings = {
    $$ = GenArray TJOper;
    KW "TJ"
  };
}

-- Text-showing operators (Table 107)
def TextShowOp (f : SizedFont) = {
  font = f;
  oper = TextShowOper;
}

def ShowStringOp (szFont : SizedFont) (s : string) : TextShowOp = {
  font = szFont;
  oper = {| showString = s |};
}

-- Text-showing operators: Table 107
def ShowTextShow (op: TextShowOp) (q : TextState) : [ UTF8 ] =
  case (op.oper: TextShowOper) of {
    showString arg -> ExtractString q op.font arg
  ; showManyStrings args ->
    for (acc = []; a in args) {
      append acc (case (a : TJOper) of {
          shownString s -> ExtractString q op.font s
        ; adjustNum -> [ ] -- TODO: possibly refine 
        })
    }
  }
