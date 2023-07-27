import Daedalus
import Lexemes

-- Reference:
--    https://datatracker.ietf.org/doc/html/rfc3986

-- This parser does limited URI normalization
--  * We expand IPv6 addresses
--  * We expand % encoded characters
--  * URI scheme is normalize to lower case
--  * URI host is normalize to lower case

-- ENTRY
def URI_absolute_URI =
  block
    scheme = URI_scheme
    $[':']
    let it    = URI_hier_part
    authority = it.authority
    rootless  = it.rootless
    path      = it.path
    query     = Optional { $['?']; URI_query }

def URI_scheme = ManyStart AlphaNoCaseLower
                           (AlphaNoCaseLower <| $[ $digit | '+' | '-' | '.'])

def URI_hier_part =
  First

    block
      Match "//"
      authority = just URI_authority
      rootless  = false
      path      = Many { $['/']; URI_segment }

    block 
      authority = nothing
      rootless  = { $['/']; false } <| true
      path      = ManyStart URI_segment_nz { $['/']; URI_segment }

def URI_segment       = Many URI_pchar
def URI_segment_nz    = Many (1..) URI_pchar



def URI_query = Many (URI_pchar <| $['"' | '?'])



--------------------------------------------------------------------------------
-- URI Authority
--------------------------------------------------------------------------------

def URI_authority =
  block
    user_info = Optional { $$ = URI_userinfo; $['@'] }
    host      = URI_host
    port      = Optional URI_port

def URI_port =
  block
    let val = many (value = DigitNum as uint 32)
                   (10 * value + (DigitNum as ?auto))
    val as? uint 16

def URI_userinfo =
  Many
    First
      $uri_unreserved
      URI_pct_encoded
      $uri_sub_delims
      $[':']

def URI_host =
  First
    IPLiteral = URI_IP_Literal
    IPv4      = URI_IPv4address
    Named     = URI_reg_name_no_case


def URI_pct_encoded =
  block
    $['%']
    16 * HexDigNum + HexDigNum

def URI_reg_name_no_case =
  Many
    First
      URI_unreserved_no_case
      URI_pct_encoded
      $uri_sub_delims


def URI_IP_Literal =
  block
    $['[']
    $$ = First
           IPv6      = URI_IPv6address
           IPvFuture = URI_IPvFuture
    $[']']


def URI_IPvFuture =
  block
    $['v']
    version = Many (1..) HexDigNum
    $['.']
    data    = Many (1..) $[ $uri_unreserved | $uri_sub_delims | ':' ]



--------------------------------------------------------------------------------
-- IP v6
--------------------------------------------------------------------------------

def URI_IPv6address =
  block
    let front =
          First

            -- Start with `::`
            block
              $[':']
              count = 0
              buf   = builder
             
            -- Has initial segments
            many (loop = { count = 0, buf = builder })
              First
                block
                  loop.count == 6 is true
                  URI_v6_last_v4 loop
                        
                block
                  loop.count < 7 is true
                  buf = emit loop.buf URI_h6
                  $[':']
                  count = loop.count + 1

    case front.count of
      8 -> build front.buf
      _ ->
        block

          let back =
                many (loop = { count = front.count
                             , buf   = builder
                             , stop  = false })
                  block
                    loop.stop is false
                    $[':']
                    First
                      block
                        loop.count < 6 is true
                        let v4 = URI_v6_last_v4 loop
                        buf    = v4.buf
                        count  = v4.count
                        stop   = true

                      block
                        buf   = emit loop.buf URI_h6
                        count = loop.count + 1
                        stop  = count == 7

          -- End in ::
          if front.count == back.count
            then @$[':']
            else Accept

          -- Only :: is not OK
          let zeros = 8 - back.count
          zeros < 8 is true

          let add_zeros =
                many (loop = { count = 0, buf = front.buf })
                  block
                    loop.count < zeros is true
                    buf   = emit loop.buf 0
                    count = loop.count + 1

          build (emitBuilder add_zeros.buf back.buf)
 
def URI_v6_last_v4 loop =
  block
    let v4  = URI_IPv4address
    let tmp = emit loop.buf (Index v4 3 # Index v4 2)
    buf     = emit tmp      (Index v4 1 # Index v4 0)
    count   = loop.count + 2 : uint 64

def URI_h6 : uint 16 =
  for (s = 0; d in Many (1 .. 4) HexDigNum) (16 * s + (d as uint 16))

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
  block
    let ds = Many (1 .. 3) DigitNum

    -- No leading 0
    (length ds == 1 || Index ds 0 > 0) is true

    -- No more than 255
    let val = for (s = 0; d in ds) (10 * s + (d as uint 16))
    val as? uint 8


--------------------------------------------------------------------------------
-- URI specifix lexical definitions

def $uri_unreserved         = $alpha | $digit | '-' | '.' | '_' | '~'
def URI_unreserved_no_case  = AlphaNoCaseLower
                           <| $[ $digit | '-' | '.' | '_' | '~' ]

def $uri_sub_delims   = '!' | '$' | '&' | '\'' | '(' | ')'
                      | '*' | '+' | ',' | ';'  | '='

def URI_pchar         = URI_pct_encoded
                     <| $[ $uri_unreserved | $uri_sub_delims | '@' | ':' ]


