-- ContentStreamLight: a lightweight parser for extracting text from
-- content streams
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import FontDict
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

-- Text-positioning operators (Table 106)
def TextPosOp = Choose1 { -- operations are mutually exclusive
  mvNextLine = {
    tx = Token Number;
    ty = Token Number;
    KW "Td"
  };
  mvNextLineSetLeading = {
    tx = Token Number;
    ty = Token Number;
    KW "TD"
  };
  setTextMatrix = {
    a = Token Number;
    b = Token Number;
    c = Token Number;
    d = Token Number;
    e = Token Number;
    f = Token Number;
    KW "Tm"
  };
  mvNextLine = KW "T*";
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
  };
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

-- TextState: state relevant to text rendering
def TextState = {
  charSpace = 0 as int;
  wordSpace = 0 as int;
  font = just FontDict;
}

def InitTextState : TextState = {
  charSpace = 0;
  wordSpace = 0;
  font = nothing;
}

def InitBytes : [ UTF8 ] = [ ]

def SetCharSpace (cs : int) (q : TextState) : TextState = {
  charSpace = cs;
  wordSpace = q.wordSpace;
  font = q.font;
}

def SetWordSpace (ws : int) (q : TextState) : TextState = {
  charSpace = q.charSpace;
  wordSpace = ws;
  font = q.font;
}

def SetFont (f : FontDict) (q : TextState) : TextState = {
  charSpace = q.charSpace;
  wordSpace = q.wordSpace;
  font = just f;
}

def TextEffect (q : TextState) (bs : [ UTF8 ]) = {
  textState = q;
  output = bs;
}

def Sequence (bs : [ UTF8 ]) (q : TextEffect) = TextEffect
  q.textState (append bs q.output)

def InitEffect = TextEffect InitTextState InitBytes

def LiftToTextEffect (q : TextState) = TextEffect q [ ]

def ExtractString (q: TextState) (s : [ uint 8 ]) = [ ]
-- TODO: define: use the font to encode bytes as UTF8

-- Text-showing operators: Table 107
def UpdTextShow (op: TextShowOp) (q : TextState) : [ UTF8 ] = case op of {
  showString -> TextEffect q (ExtractString q s)
; mvNextLineShow s -> ContentStreamEffect [
      {| textPosOp = {| mvNextLine = {} |} |};
    , {| textShowOp = {| showString = s |} |}
    ]
    q
-- TODO: parse this into directly into the sequence of operands
; mvNextLineShowSpacedString x -> ContentStreamEffect [
      {| textStateOp = {| setWordSpace = x.aw |} |}
    , {| textStateOp = {| setCharSpace = x.ac |} |}
    , {| textShowOp = {| mvNextLineShow = x.string |} |}
    ]
    q
; showManyStrings args -> Void -- TODO: define
}

def UpdTextState (op: TextStateOp) (q: TextState) : TextState = case op of {
  setCharSpace csOpers -> SetCharSpace csOpers.charSpace q
; setWordSpace wsOpers -> SetWordSpace wsOpers.wordSpace q
; setScale -> q
; setLeading -> q
; setFont sf -> SetFont sf.font q
; setRenderingMode rm -> q
; setRise rs -> q
}

-- TODO: interpret TJ as inserting a newline, per PDF court

def UpdPos (op: TextPosOp) (q : TextState) : TextEffect = case op of {
  mvNextLine x -> TextEffect q [ UTF8Ascii '\n' ]
; mvNextLineSetLeading x -> ContentStreamEffect
    [ {| textStateOp = {|
           setLeading = x.ty;
      |}
    , {| textShowOp = {|
           mvNextLine = {
             tx = x.tx;
             ty = x.ty;
           }
         |}
      |}
    ]
    q 
; setTextMatrix m -> LiftToTextEffect q
; mvNextLine -> UpdPos {| mvNextLineSetLeading = 0 |} q -- TODO: refine?
}

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

