-- ContentStreamLight: a lightweight parser for extracting text from
-- content streams
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import FontDict
import ResourceDict
import TextObj
import TextPosOp
import TextShowOp
import TextStateOp
import Unicode

-- ContentStreamOp: an operation in a content stream
def ContentStreamOp resourceD = Choose1 {
  textObj = TextObj resourceD;
  unparsedByte = UInt8;
}

-- InterpContentStream: interpret a content stream, resolving lookups
-- into the resource dictionary.
def InterpContentStream (resourceD : ResourceDict) =
  Many (ContentStreamOp resourceD)

def ContentStreamEffect (cs : [ ContentStreamOp ]) (q0 : TextState) :
  TextEffect = 
  for (acc = q0; op in cs) (case op of {
    textPosOp posOp -> Sequence
      (UpdPos posOp acc.textState) acc.output;
  ; textStateOp stateOp -> TextEffect
      (UpdTextState stateOp acc.textState) acc.output;
  ; textShowOp showOp -> TextEffect
      q (append acc.output (UpdTextShow showOp acc.textState))
  ; unparsedByte -> acc;
  })

-- ExtractContentStreamText cs: extract text from content stream cs
def ExtractContentStreamText (cs : [ ContentStreamOp ]) : [ UTF8 ] =
  (ContentStreamEffect cs InitTextState).output

-- TODO: finish definition. Involves extending state that is
-- accumulated in the for loop.

-- DEPRECATED:

-- ShowVal: a value that can be shown
def ShowVal =
  String <|
  { Number;
    ^ " "
  }

-- Text-showing operators (Table 107)
def TextShowOp1 = 
  { $$ = Token String;
    KW "Tj" 
  } <|
  { $$ = Token String;
    KW "'" 
  } <|
  { Token Number ; -- a_w
    Token Number ; -- a_c
    $$ = Token String;
    KW "\"" 
  } <|
  { @arr = Token (GenArray (Token ShowVal)) ;
    KW "TJ";
    ^ (concat arr)
  } 

-- ContentStream: a simple parser that never fails and returns all
-- text in showing operators. Deprecated
def ContentStream = {
  @shownStrings = Many {
    -- parse an object that shows text;
    @s = TextShowOp1 <|
      -- just eat a byte
      { UInt8;
        ^ ""
      };
    WithStream (arrayStream s) (Many UnicodeByte)
  };
  ^(concat shownStrings)
}

