-- TextPosOp: text positioning operators
import Stdlib
import GenPdfValue
import PdfValue

import TextEffect
import Unicode

-- Text-positioning operators (Table 106)
def TextPosOp = Choose1 { -- operations are mutually exclusive
}

-- TODO: Td sets the text matrix, so does Tm. If we model Td, should
-- we model Tm?

def MvNextLineOp (x : int) (y : int) : TextPosOp = {| mvNextLine = {
  tx = x;
  ty = y;
} |}

def MvNextLineStart = {| mvNextLineStart = {} |}

def UpdPos (op: TextPosOp) (q : TextState) : TextEffect = case op of {
  mvNextLine x -> TextEffect q [ UTF8Ascii '\n' ]
; setTextMatrix m -> LiftToTextEffect q -- TODO: refine
; mvNextLineStart -> UpdPos {| mvNextLineSetLeading = 0 |} q -- TODO: refine?
}
