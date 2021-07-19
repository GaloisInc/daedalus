import ResourceDict

import TextShowOp
import TextStateOp
import TextPosOp

-- TextOp: a text operation
def TextOp (rd: ResourceDict) = Choose1 {
  -- all text operands are mutually exclusive
  textShowOp = TextShowOp;
  textStateOp = TextStateOp rd;
  textPosOp = TextPosOp;
}

def MvNextLineWOffset (tx : int) (ty : int) = 

-- TextOpP: a parser for a sequence of text operations
def TextOpP : [ TextOp ] = (LiftPToArray TextOp) <|
  -- text operations that are basically macros over other
  -- operations. TD operations: (Table 106)
  { @tx = Token Integer;
    @ty = Token Integer;
    KW "TD"; -- Table 106
    [ {| textStateOp = SetLeadingOp ty |}
    , {| textShowOp = MvNextLineOp tx ty |}
    ]
  } <|
  { KW "'"; -- Table 107
    [ {| textPosOp = MvNextLineStart |}
    , {| textShowOp = ShowStringOp |}
    ]
  } <|
  { @aw = Token Number;
    @ac = Token Number;
    @str = Token String;
    KW "\""; -- Table 107
    [ {| textStateOp = SetWordSpaceOp aw |}
    , {| textStateOp = SetCharSpaceOp ac |}
    , {| textPosOp = MvNextLineStart |}
    ]
  }

-- TextObj: a text object
def TextObj (rd: ResourceDict) = Between "BT" "ET" (concat (Many TextOpP))
