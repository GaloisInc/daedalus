-- ContentStreamLight: a lightweight parser for extracting text from
-- content streams
import Stdlib
import Array
import Pair

import GenPdfValue
import PdfValue
import FontDict
import ResourceDict
import TextEffect
import TextObj
import TextShowOp
import TextStateOp
import Unicode

-- ContentStreamOp: an operation in a content stream
def ContentStreamOp (rd : ResourceDict) (f: maybe SizedFont) = Choose1 {
  textObj = TextObj rd f; -- text object
  textStateOp = TextStateOp; -- text state operators outside of object
}

def CSOpOrByte (rd : ResourceDict) (f : maybe SizedFont) :
  maybe ContentStreamOp =
  OrEatByte (ContentStreamOp rd f)

-- ContentStreamP: interpret a content stream, resolving lookups
-- into the resource dictionary.
def ContentStreamP (rd : ResourceDict) =
  optionsToArray (OpsWithFont (CSOpOrByte rd) rd nothing)

def ContentStreamEffect (cs : [ ContentStreamOp ]) (q0 : TextState) :
  TextEffect = {
  @eff0 = InitEffect;
  for (effAcc = eff0; op in cs) {
    case op of {
      textObj obj -> Sequence
        effAcc
        (InterpTextObj obj effAcc.textState)
    ; textStateOp tsOper -> SetEffectState
        (UpdTextState tsOper effAcc.textState)
        effAcc
    }
  }
}

-- ExtractContentStreamText cs: extract text from content stream cs
def ExtractContentStreamText (cs : [ ContentStreamOp ]) : [ UTF8 ] =
  (ContentStreamEffect cs InitTextState).output

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
-- text in showing operators
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

