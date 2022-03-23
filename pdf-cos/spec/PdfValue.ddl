--------------------------------------------------------------------------------
-- The declarations in this module are all related to Section 7
--------------------------------------------------------------------------------
import Stdlib


--------------------------------------------------------------------------------
-- White Space (Section 7.2)

def $lf                   = 10
def $cr                   = 13
def $space                = 32
def $simpleWS             = 0 | 9 | 12 | 32

-- Beware: this is rarely used!  PDF uses only after 'stream' keyword
def SimpleEOL             = @($lf <| { $cr; $lf })

def EOL : {}              = @$lf <| { $cr; @$lf <| Accept }

def GenComment start =
  block
    $['%']
    Match start
    SkipTillEOL

def Comment               = GenComment ""
def JustWhite             = @$simpleWS <| EOL     -- no comments
def AnyWS                 = JustWhite <| Comment

-- and sequences of ...
def ManyJustWhite         = @Many JustWhite
def ManyWS                = @Many AnyWS



--------------------------------------------------------------------------------
-- Boolean Objects (Section 7.3.2)

def Bool =
     When (KW "true")  true
  <| When (KW "false") false


--------------------------------------------------------------------------------
-- Numberic Objects (Section 7.3.3)

def Number = Token
  block
    let sign = Sign
    let n    = UnsignedNumber;
    case sign of
      pos -> n
      neg -> { num = 0 - n.num; exp = n.exp }

def intNumber (n : int) : Number =
  block
    num = n
    exp = 0

def Sign =
  First
    neg = @Match "-"
    pos = @Optional (Match "+")

def UnsignedNumber =
     UnsignedLeadDigits
  <| Frac 1 { num = 0, exp = 0 }

def UnsignedLeadDigits =
  block
    let n   = Natural
    let val = { num = n; exp = 0 : int }
    Frac 0 val  <| val
    -- biased choice here is important
    -- otherwise 1.2 can be processed as "1.2" or "1" followed by ".2"

def Natural = numBase 10 (Many (1..) Digit)

def Frac n (w : Number) : Number =
  block
    $['.']
    for (val = w; d in Many (n ..) Digit)
      { num = 10 * val.num + d; exp = val.exp - 1 }

-- XXX: no need to use big ints here.
-- at the moment we do it to avoid overflowing numbers when parsing in base 10
def OctDigit  = $['0' .. '7'] - '0' as int
def Digit     = $['0' .. '9'] - '0' as int
def HexDigit  =  Digit
              <| 10 + $['a' .. 'f'] - 'a' as int
              <| 10 + $['A' .. 'F'] - 'A' as int

def NumberAsNat (x : Number) =
  block
    Guard (x.num >= 0 && x.exp == 0)
    x.num

--------------------------------------------------------------------------------
-- parsing the first 2 lines of PDF, the header

def Header =
  block
    Match "%PDF-"

    First
      block Match "1."; @$['0' .. '7']
      @Match "2.0"

    SkipTillEOL
    case Optional BinaryMarker of
      just    -> true         -- a binary file is indicated
      nothing -> false

def BinaryMarker =
  block
    Match "%"
    Many (4..) $[ 128 .. ]
    SkipTillEOL

def SkipTillEOL =
  block
    Many $[! ($lf | $cr)] -- cavity
    EOL

--------------------------------------------------------------------------------
-- Literal Strings (Section 7.3.4.2)


{- NOTE: it is important that we don't use Between here, because we
don't want to skip space after the first paren: it is part of the string -}
def String =
  block
    $['(']
    $$ = StringChars
    Token $[')']

def StringChars = concat (Many StringChunk)

def StringChunk =
  First
    StringInParens
    StringEsc
    Many (1..) $[! "\\()"]

def StringInParens = concat [ Match "(", StringChars, Match ")" ]

def StringEsc =
  block
    $['\\']
    First
      When $['n']   "\n"
      When $['r']   "\r"
      When $['t']   "\t"
      When $['b']   "\8"
      When $['f']   "\12"
      Match "("
      Match ")"
      Match "\\"
      When EOL      ""
      StringNumEsc

def StringNumEsc = [ numBase 8 (Many (1..3) OctDigit) as! uint 8 ]



--------------------------------------------------------------------------------
-- Hexadecimal Strings (Sections 7.3.4.3)

def HexString = Between "<" ">"
  block
    let front = Many HexStringNum2
    concat [ front, [HexStringNum1] ] <| front

def HexStringNum2 = numBase 16 (Many 2 (Token HexDigit)) as! uint 8
def HexStringNum1 = 16 * Token HexDigit as! uint 8


--------------------------------------------------------------------------------
-- Name Objects (Sections 7.3.5)

def Name =
  Token
    block
      $['/']
      Many NameChar

def NameChar =   Match1 (!"\0\9\10\12\13\32()<>[]{}/%#")
              <| NameEsc

def NameEsc  =
  block
    $['#']
    $$ = numBase 16 (Many 2 HexDigit) as! uint 8
    $$ > 0 is true


--------------------------------------------------------------------------------
-- Array Objectcs (Section 7.3.6)

def Array = Between "[" "]" (Many Value)


--------------------------------------------------------------------------------
-- Dictionary Objects (Section 7.3.7)

-- NOTE: we relax the spec to allow repeated keys, using the latest value.
def Dict =
  block
    let ents = Between "<<" ">>" (Many { key = Name; value = Value })
    for (d = empty; e in ents) (insert e.key e.value d)


def Null  = KW "null"

def Ref =
  block
    obj = Token Natural
    gen = Token Natural
    KW "R"


-- Objects ---------------------------------------------------------------------

def Value =
  First
    null    = Null
    bool    = Bool
    ref     = Ref      -- This must come before number, as they overlap
    name    = Name
    string  = String
    string  = HexString
    number  = Number
    array   = Array
    dict    = Dict

def NatValue (v : Value) = NumberAsNat (v is number)

def nullValue : Value = {| null = {} |}


--------------------------------------------------------------------------------
-- Helpers

def Token P               = block $$ = P; Many AnyWS
def KW x                  = Token (@Match x)
def Between open close P  = block KW open; $$ = P; KW close


