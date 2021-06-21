-- ContentStreamLight: a lightweight parser for extracting text from
-- content streams
import Stdlib
import PdfValue

import Unicode

-- GenArray P: general array of P's
def GenArray P = Between "[" "]" (Many P)

-- ShowVal: a value that can be shown
def ShowVal =
  String <|
  { Number;
    ^ " "
  }

-- Text-showing operators (Table 107)
def TextShowOp = 
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
    @s = TextShowOp <|
      -- just eat a byte
      { UInt8;
        ^ ""
      };
    UnicodeString CharCodeToUnicode s
  };
  ^(concat shownStrings)
}
