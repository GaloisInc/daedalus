-- ContentStreamLight: a lightweight parser for extracting text from
-- content streams
import Stdlib

import GenPdfValue
import PdfValue
import ResourceDict
import Unicode

-- ShowVal: a value that can be shown
def ShowVal =
  String <|
  { Number;
    ^ " "
  }

-- Text state operators (Table 103)
def TextStateOp resrcs = Choose1 {
  setCharSpace = {
    $$ = Token Number;
    KW "Tc"
  };
  setWordSpace = {
    $$ = Token Number;
    KW "Tw"
  };
  setScale = {
    $$ = Token Number;
    KW "Tz"
  };
  setLeading = {
    $$ = Token Number;
    KW "TL"
  };
  setFont = {
    @fontNm = Token Name;
    font = Lookup fontNm resrcs.font;
    size = Token Number;
    KW "Tf"
  };
  setRenderingMode = {
    $$ = Token Number; -- TODO: refine to integer
    KW "Tr"
  };
  setRise = {
    $$ = Token Number;
    KW "Ts"
  }
}

-- Text-showing operators (Table 107)
def TextShowOp = Choose1 { -- operations are mutually exclusive:
  showString = {
    $$ = Token String;
    KW "Tj" 
  };
  mvNextLineShow = {
    $$ = Token String;
    KW "'" 
  };
  mvNextLineShowSpacedString = {
    aw = Token Number;
    ac = Token Number;
    str = Token String;
    KW "\"" 
  };
  showManyStrings = {
    $$ = GenArray (Choose {
      shownString = String;
      adjustNum = Number;
    });
    KW "TJ"
  }
}

def ContentStreamOp resourceD = Choose1 {
  textStateOp = TextStateOp resourceD;
  textShowOp = TextShowOp;
  unparsedByte = UInt8;
}

-- InterpContentStream: interpret a content stream, resolving lookups
-- into the resource dictionary.
def InterpContentStream (resourceD : ResourceDict) =
  Many (ContentStreamOp resourceD)

-- ExtractContentStreamText cs: extract text from content stream cs
def ExtractContentStreamText (cs : [ ContentStreamOp ]) =
  for (str = ""; op in cs) {
    @opStr = case op of {
      textStateOp -> "text state op";
      textShowOp -> "text show op";
      -- TODO: per PDF Court, interpret TJ as inserting a newline
      unparsedByte -> "";
    };
    append str opStr
  }

-- TODO: finish definition. Involves extending state that is
-- accumulated in the for loop.

-- DEPRECATED:

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

