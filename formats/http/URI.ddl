import Lexemes

-- Reference:
--	https://datatracker.ietf.org/doc/html/rfc3986#appendix-A

def URI =
  block
    scheme = URI_scheme


def URI_scheme =
  build (many (buf = emit builder $alpha)
              (emit buf $[$alpha | $digit | '+' | '-' | '.']))


def URI_userinfo =
  Many 
    First
      $uri_unreserved
      URI_pct_encoded
      $uri_sub_delims
      $[':']


def $uri_unreserved = $alpha | $digit | '-' | '.' | '_' | '~'
def $uri_sub_delims = '!' | '$' | '&' | '\'' | '(' | ')'
                    | '*' | '+' | ',' | ';'  | '='

def URI_pct_encoded =
  block
    $['%']
    16 * HexDigNum + HexDigNum

def URI_reg_name =
  Many 
    First
      $uri_unreserved
      URI_pct_encoded
      $uri_sub_delims


--------------------------------------------------------------------------------
-- IP v6
--------------------------------------------------------------------------------

-- XXX: Unfinished.  Simpler way to re
{-
def URI_IPv6address =
  block
    let before = many (s = { count = 0; buf = builder })
                    block
                      count < 7 is true
                      buf = emit buf URI_h6
                      $[':']
                      count = count + 1

    let end_before = if before.count > 0
                        then { count = before.count + 1
                             , buf   = emit before.buf URI_h6
                             }
                        else before

    if end_before.count < 8
      then
        First

          block
            end_before.count == 6 is true
            let ipv4 = URI_IPv6address
            Fail "XXX: last 2 digits come from IPv4" 

          block
            Match "::"
            let lim   = 8 - end_before.count - 1{-zero-}
            let after = Many (.. lim) { $$ = URI_h6; $[':'] }


            let zeros = 6 - end_before.count - length after
            let addZeros = many ({ buf = end_before.buf, todo = zeros })
                             block
                                todo > 0 is true
                                buf  = emit buf 0
                                todo = todo - 1
            build (for (buf = addZeros.buf; x in after) (emit buf x))
                     
                    
      else end_before.buf
-}

def URI_h6 : uint 16 =
  (16 * HexDigNum + HexDigNum) # (16 * HexDigNum + HexDigNum)

--------------------------------------------------------------------------------
-- IP v4
--------------------------------------------------------------------------------

def URI_IPv4address : [ uint 8 ] =
  block
    let a = URI_dec_octet
    $['.']
    let b = URI_dec_octet
    $['.']
    let c = URI_dec_octet
    $['.']
    let d = URI_dec_octet
    [ a, b, c, d ]
    
   
def URI_dec_octet =
  First
    { Match "25";   250 + Decimal $['0' .. '5'] }
    { $['2'];       200 + 10 * Decimal $['0' .. '4'] + DigitNum }
    { $['1'];       100 + 10 * DigitNum + DigitNum }
    {                     10 * Decimal $['1' .. '9'] + DigitNum }
    DigitNum


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------


def Main =
  block
    SetStream (arrayStream "000F")
    URI_h6


