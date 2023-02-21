-- HTTP 1.1
-- Reference: https://datatracker.ietf.org/doc/html/rfc9112#appendix-A

import Utils
import Lexemes
import URI

-- ENTRY
def HTTP_request = HTTP_message HTTP_request_line

-- ENTRY
def HTTP_status  = HTTP_message HTTP_status_line

def HTTP_message StartLine =
  block
    start = StartLine
    CRLF
    fields = Many { $$ = HTTP_field_line; CRLF }
    CRLF
    body = GetStream

def HTTP_version =
  block
    Match "HTTP/"
    major = DigitNum
    $['.']
    minor = DigitNum

def HTTP_status_line =
  block
    version = HTTP_version
    $sp
    status_code = Many 3 DigitNum
    $sp
    reason = Many $[ $htab | $sp | $vchar | $obs_text ]

def HTTP_request_line =
  block
    method = HTTP_method
    $sp
    target = HTTP_request_target
    $sp
    version = HTTP_version

def HTTP_request_target =
  First
    AbsoluteURI = URI_absolute_URI        -- old style and for proxies
    Origin      = HTTP_origin_form        -- normal request
    Authority   = HTTP_authority_form     -- only in CONNECT
    Asterisk    = @$['*']                 -- only in OPTIONS

def HTTP_authority_form =
  block
    host  = URI_host
    $[':']
    port  = Many DigitNum

def HTTP_origin_form =
  block
    path  = Many (1..) { $['/']; URI_segment }
    query = Optional { $['?']; URI_query }

def HTTP_method =
  First
    GET     = @Match "GET"
    HEAD    = @Match "HEAD"
    POST    = @Match "POST"
    PUT     = @Match "PUT"
    DELETE  = @Match "DELETE"
    CONNECT = @Match "CONNECT"
    OPTIONS = @Match "OPTIONS"
    TRACE   = @Match "TRACE"

def HTTP_field_line =
  block
    name = HTTP_token
    $[':']
    HTTP_OWS
    value =
      block
        let start = GetStream
        Take HTTP_field_content start

def HTTP_field_content =
  many (count = 0)
    block
      let n = HTTP_OWS
      $http_field_vchar
      count + n + 1


--------------------------------------------------------------------------------
-- Field Values
--------------------------------------------------------------------------------

def HTTP_quoted_string =
  block
    $dquote
    $$ = Many ( $[$htab | $sp | 0x21 | 0x23 .. 0x5B | 0x5D .. 0x7E | $obs_text]
             <| HTTP_quoted_pair
              )
    $dquote

def HTTP_quoted_pair =
  block
    $['\\']
    $[ $htab | $sp | $vchar | $obs_text ]

def HTTP_comment =
  block
    $['(']
    Many ( @ $[ $htab | $sp | 0x21 .. 0x27 | 0x2A .. 0x5B | 0x5D .. 0x7E
                                                               | $obs_text ]
        <| @HTTP_quoted_pair
        <| HTTP_comment
         )
    $[')']
    Accept

def $http_ctext = $htab | $sp | 0x21 .. 0x27 | 0x2A .. 0x5


--------------------------------------------------------------------------------
-- HTTP Lexical Considerations
--------------------------------------------------------------------------------

-- Returns how many white spaces were skipped
def HTTP_OWS   = Count $[ $sp | $htab]
def HTTP_token = Many (1..) $http_tchar


def $http_field_vchar = $vchar | $obs_text

def $http_tchar = 
  '!'  | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' |
   '^' | '_' | '`' | '|' | '~' | $digit | $alpha


