-- HTTP 1.1
-- Reference: https://datatracker.ietf.org/doc/html/rfc9112#appendix-A

import Utils
import Lexemes
import URI


def HTTP_message =
  block
    start = HTTP_start_line
    CRLF
    fields = Many { HTTP_field_line; CRLF }
    CRLF
    body = GetStream

def HTTP_start_line =
  First
    Status  = HTTP_status
    Request = HTTP_request

def HTTP_version =
  block
    Match "HTTP/"
    major = DigitNum
    $['.']
    minor = DigitNum

def HTTP_status =
  block
    version = HTTP_version
    $sp
    status_code = Many 3 DigitNum
    $sp
    reason = Many $[ $htab | $sp | $vchar | $obs_text ]

def HTTP_request =
  block
    method = HTTP_token
    $sp
    target = HTTP_request_target
    $sp
    version = HTTP_version

def HTTP_request_target =
  First
    AbsoluteURI = URI_absolute_URI
    Origin      = HTTP_origin_form
    Authority   = HTTP_authority_form
    Asterisk    = @$['*']

def HTTP_authority_form =
  block
    host  = URI_host
    $[':']
    post  = Many DigitNum

def HTTP_origin_form =
  block
    path  = Many (1..) { $['/']; URI_segment }
    query = Optional { $['?']; URI_query }


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
-- HTTP Lexical Considerations

-- Returns how many white spaces were skipped
def HTTP_OWS   = Count $[ $sp | $htab]
def HTTP_token = Many (1..) $http_tchar


def $http_field_vchar = $vchar | $obs_text

def $http_tchar = 
  '!'  | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' |
   '^' | '_' | '`' | '|' | '~' | $digit | $alpha



def Main =
  block
    SetStream (arrayStream "hello world  a   ")
    HTTP_field_content


