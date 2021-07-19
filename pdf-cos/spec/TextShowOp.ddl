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
}

def ShowStringOp (s : string) = {| showString = s |}

-- Text-showing operators: Table 107
def UpdTextShow (op: TextShowOp) (q : TextState) : [ UTF8 ] = case op of {
  showString s -> ExtractString q s
; showManyStrings args ->
  for (acc = []; a in args) {
    append acc (case (a : TJOper) of {
      shownString s -> ExtractString q s
    ; adjustNum -> [ ] -- TODO: possibly refine for text extraction
    })
  }
}
