--------------------------------------------------------------------------------
-- The declarations in this module are all related to Section 7
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- White Space (Section 7.2)

def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } <| $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { Match "%"; Many (Match1 (! ($lf | $cr))); EOL }
def AnyWS                 = $simpleWS <| Comment <| EOL
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Helpers

def Token P               = { $$ = P; Many AnyWS }
def KW x                  = @ (Token (Match x))
def Between open close P  = { KW open; $$ = P; KW close }
def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def Only P                = { $$ = P; END }
def When P x              = { P; ^ x }
def Guard p               = p is true
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Boolean Objects (Section 7.3.2)

def Bool =
     When (KW "true")  true
  <| When (KW "false") false


--------------------------------------------------------------------------------
-- Numberic Objects (Section 7.3.3)

def Number = Token {
  @sign = Sign;
  @n    = UnsignedNumber;
  case sign is {
    pos -> n;
    neg -> { num = 0 - n.num; exp = n.exp };
  }
}

def Sign = Choose1 {
  neg = @Match "-";
  pos = @Match "+";
  pos = {};
}

def UnsignedNumber = UnsignedLeadDigits <| Frac 1 { num = 0, exp = 0 }

def UnsignedLeadDigits = {
  @n   = Natural;
  @val = ^ { num = n; exp = 0 : int };
  Frac 0 val  <| (^ val)
  -- biased choice here is important
  -- otherwise 1.2 can be processed as "1.2" or "1" followed by ".2"
}

def Natural = numBase 10 (Many (1..) Digit)

def Frac n (w : Number) : Number = {
  Match ".";
  @ds = Many (n ..) Digit;
  ^ for ( val = w; d in ds)
          { num = 10 * val.num + d; exp = val.exp - 1 }
}

def OctDigit  = Match1 ('0' .. '7') - '0' as int
def Digit     = Match1 ('0' .. '9') - '0' as int
def HexDigit  =
  Choose1 {
    Digit;
    10 + Match1 ('a' .. 'f') - 'a' as int;
    10 + Match1 ('A' .. 'F') - 'A' as int;
  }

def NumberAsNat (x : Number) = { Guard (x.num >= 0 && x.exp == 0); ^ x.num }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Literal Strings (Section 7.3.4.2)

def String = Between "(" ")" StringChars

def StringChars = concat (Many StringChunk)

def StringChunk =
    StringInParens
 <| StringEsc
 <| Many (1..) (Match1 (! "\\()"))

def StringInParens = concat [ Match "(", StringChars, Match ")"]

def StringEsc = {
   Match "\\";
   Choose1 {
     When (Match "n")   "\n";
     When (Match "r")   "\r";
     When (Match "t")   "\t";
     When (Match "b")   "\8";
     When (Match "f")   "\12";
     When (Match "(")   "(";
     When (Match ")")   ")";
     When (Match "\\")  "\\";
     When EOL           "";
     StringNumEsc
  }
}

def StringNumEsc = [ numBase 8 (Many (1..3) OctDigit) as! uint 8 ]



--------------------------------------------------------------------------------
-- Hexadecimal Strings (Sections 7.3.4.3)

def HexString = Between "<" ">" {
  @front = Many HexStringNum2;
  concat [ front, [HexStringNum1] ] <| front
}

def HexStringNum2 = numBase 16 (Many 2 (Token HexDigit)) as! uint 8
def HexStringNum1 = 16 * Token HexDigit as! uint 8


--------------------------------------------------------------------------------
-- Name Objectcs (Sections 7.3.5)

def Name     = Token { Match "/"; Many NameChar }

def NameChar = Match1 (!"\0\9\10\12\13\32()<>[]{}/%#")
            <| NameEsc

def NameEsc  = {
  Match "#";
  $$ = numBase 16 (Many 2 HexDigit) as! uint 8;
  Guard ($$ > 0)
}



--------------------------------------------------------------------------------
-- Array Objectcs (Section 7.3.6)

def Array = Between "[" "]" (Many Value)


--------------------------------------------------------------------------------
-- Dictionary Objects (Section 7.3.7)

def Dict = {
  @ents = Between "<<" ">>" (Many { key = Name; value = Value });
  for (d = empty; e in ents) (Insert e.key e.value d)
}


def Null  = KW "null"

def Ref = {
  obj = Token Natural;
  gen = Token Natural;
  KW "R"
}


-- Objects ---------------------------------------------------------------------

def Value = 
  Choose1 {
    null    = Null;
    bool    = Bool;
    ref     = Ref;      -- This must come before number, as they overlap
    name    = Name;
    string  = String;
    string  = HexString;
    number  = Number;
    array   = Array;
    dict    = Dict;
  }

def NatValue (v : Value) = {
  @n = v is number;
  NumberAsNat n;
}

def nullValue : Value = {| null = {} |}

