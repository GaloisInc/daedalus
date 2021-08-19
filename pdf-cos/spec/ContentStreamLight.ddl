-- ContentStreamLight: a lightweight parser for extracting text from
-- content streams
import Stdlib
import Array
import Pair

import GenPdfValue
import ColourSpaceOps
import GraphicsStateOps
import MarkedContentOps
import PdfValue
import FontDict
import ResourceDict
import TextEffect
import TextObj
import TextShowOp
import TextStateOp
import Unicode

-- ContentStreamOp: an operation in a content stream
def ContentStreamOp = Choose1 {
  textStateOp = TextStateOp; -- text state operators outside of object
  graphicsStateOp = GraphicsStateOp;
  colourOp = ColourOp;
  markedPoint = MarkContentPoint;
  markedSeq = MarkedContentSeqOp;
}

def ContentStreamObj = Choose1 {
  textObj = [] : [ TextOp ]
; csOp = ContentStreamOp
}

def CSTextObj (tobj : [ TextOp ]) : ContentStreamObj = {|
  textObj = tobj
|}

def CSOp (op : ContentStreamOp) : ContentStreamObj = {|
  csOp = op
|}

def LiftedTextObj (rd: ResourceDict) (szFont: maybe sizedFont) :
  FontEffect = {
  @eff0 = TextObj rd szFont;
  FontEffect eff0.fst (just (just (CSTextObj eff0.snd))) -- ?
}

-- ContentStreamP: parse a content stream, resolving lookups
-- into the resource dictionary.
def ContentStreamP (rd : ResourceDict) : [ ContentStreamObj ] = {
  @eff0 = ManyWithState (GenSum1
    (FontOpEffect rd)
    (GenSum1 (LiftedTextObj rd)
      (LiftResToFontEffect (Const (OrEatByte (CSOp ContentStreamOp)))) ))
    nothing;
  optionsToArray eff0.snd
}

-- interpret a sequence of operators as a text effect
def ContentStreamEffect (cs : [ ContentStreamObj ]) (q0 : textState) :
  textEffect = {
  @eff0 = initEffect;
  for (effAcc = eff0; op in cs) {
    case op of {
      textObj obj -> sequence
        effAcc
        (InterpTextObj obj effAcc.textState)
    ; csOp csOper -> case csOper of {
        textStateOp tsOper -> setEffectState
          (updTextState tsOper effAcc.textState)
          effAcc
      ; _ -> effAcc -- treat all other operations as noops, for now
      }
    }
  }
}

-- ExtractContentStreamText cs: extract text from content stream cs
def ExtractContentStreamText (cs : [ ContentStreamObj ]) : [ UTF8 ] =
  (ContentStreamEffect cs initTextState).output

-- TODO: parse other operators

-- TODO: thread the font through streams

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
    WithStream (arrayStream s) (Many UTF8AsciiP)
  };
  ^(concat shownStrings)
}

