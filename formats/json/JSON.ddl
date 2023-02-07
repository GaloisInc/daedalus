import utf8

--------------------------------------------------------------------------------
-- Values

def JSON_value =
  First
    Null    = JSON_null
    Bool    = JSON_bool
    Number  = JSON_number
    String  = JSON_string
    Array   = JSON_array_of JSON_value
    Object  = JSON_object_of JSON_value


--------------------------------------------------------------------------------
-- Objects

def JSON_object_of P =
  block
    $['{']
    let mp =
         case Optional (JSON_ws_then (JSON_field_of P)) of
           nothing -> empty
           just v  -> insert v.key v.value empty
    $$ = many (m = mp)
           block
             JSON_ws_then $[',']
             let f = JSON_ws_then (JSON_field_of P)
             if ?strict
               then Insert f.key f.value m
               else insert f.key f.value m
    JSON_ws_then $['}']

def JSON_field_of P =
  block
    key = JSON_string
    JSON_ws_then $[':']
    value = JSON_ws_then P

--------------------------------------------------------------------------------
-- Arrays

def JSON_array_of P =
  block
    $['[']
    let buf =
         case Optional (JSON_ws_then P) of
           nothing -> builder
           just v  -> emit builder v
    $$ = build
           (many (buf = buf)
              block
                JSON_ws_then $[',']
                emit buf (JSON_ws_then P)
           )
    JSON_ws_then $[']']


--------------------------------------------------------------------------------
def JSON_ws_then P =
  block
    Many $json_ws
    P

def $json_ws  = 0x20 | 0x0A | 0x0D | 0x09
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Booleans

def JSON_bool =
  First
    { Match "true"; true }
    { Match "false"; false }


--------------------------------------------------------------------------------
def JSON_null = @Match "null"



--------------------------------------------------------------------------------
-- Numbers


def JSON_number =
  block
    sign     = JSON_sign
    let nat  = JSON_Natural
    let frac = JSON_Fraction nat
    whole = frac.val
    exp = JSON_exponent + frac.exp

def JSON_sign =
  First
    Neg = @$['-']
    Pos = Accept

def JSON_sign_plus = JSON_sign <| { $['+']; {| Pos |} }

def json_sign (s : JSON_sign) v =
  case s of
    Pos -> v
    Neg -> -v

def JSON_Natural =
  block
    let d = JSON_Digit
    if d == 0
      then d
      else many (value = d) (value * 10 + JSON_Digit)

def JSON_Fraction base =
  block
    let default = { exp = 0, val = base }
    case Optional (@$['.']) of
      just ->
        block
          $$ = many (s = default)
                 block
                   exp = s.exp - 1 : int
                   val = 10 * s.val + JSON_Digit
          $$.exp > 0 is true
      nothing -> default

def JSON_exponent : int =
  case Optional $['E' | 'e'] of
    just    -> json_sign JSON_sign_plus
                         (many (value = JSON_Digit) (value * 10 + JSON_Digit))
    nothing -> 0

def JSON_Digit  = $['0' .. '9'] - '0' as int
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Strings (in UTF8)

def JSON_Hex : uint 32 =
  First
    $['0' .. '9'] - '0'
    10 + ($['a' .. 'f'] - 'a')
    10 + ($['A' .. 'F'] - 'A')
  as ?auto



def JSON_string =
  block
    $['"']
    let buf =
          many (s = { str = builder, end = false })
            if s.end
              then Fail ""
              else
                block
                  let c = UTF8
                  case c of  
                    34 {- " -} -> { str = s.str, end = true }
                    92 {- \ -} -> { str = JSON_escape s.str, end = false }
                    _          -> { str = EmitUTF8 s.str c, end = false }
    if buf.end
      then build buf.str
      else Fail "Unterminated string"

def JSON_escape s =
  case $any of
    '"'   -> emit s '"'
    '\\'  -> emit s '\\'
    '/'   -> emit s '/'
    'b'   -> emit s 8
    'f'   -> emit s 12
    'n'   -> emit s '\n'
    'r'   -> emit s '\r'
    't'   -> emit s '\t'
    'u'   -> EmitUTF8 s (4096 * JSON_Hex +
                          256 * JSON_Hex +
                           16 * JSON_Hex +
                                JSON_Hex)



