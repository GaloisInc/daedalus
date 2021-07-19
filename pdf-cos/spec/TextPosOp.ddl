-- TextPosOp: text positioning operators
import Stdlib
import GenPdfValue
import PdfValue

import TextEffect
import Unicode

-- Text-positioning operators (Table 106)
def TextPosOp = Choose1 { -- operations are mutually exclusive
  mvNextLine = { -- updates only ouptut stream. TODO: refactor to Text Show
    tx = Token Integer;
    ty = Token Integer;
    KW "Td"
  };
  setTextMatrix = { -- updates text matrix, which affects user space
    -- TODO: refactor to ??
    a = Token Number;
    b = Token Number;
    c = Token Number;
    d = Token Number;
    e = Token Number;
    f = Token Number;
    KW "Tm"
  };
  mvNextLineStart = KW "T*"; -- updates stream and state. TODO: hoist this to text ops
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
