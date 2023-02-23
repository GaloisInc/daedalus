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
    let ty = HTTP_body_type fields
    body = HTTP_message_body ty

def HTTP_body_type (fields : [HTTP_field_line]) =
  block
    let none = { chunked = false; len = 0 }

    for (result = none; f in fields)
      case f: HTTP_field_line of
        Header _ -> ^ result

        Content_Length h ->
          -- Chunked encoding takes precedence over Content-Length, so
          -- only store the length if we haven't already found a chunked
          -- encoding header.
          if result.chunked
            then ^ result
            else ^ { chunked = false; len = h.value }

        Transfer_Encoding h ->
          for (result2 = result; entry in h.encodings)
            if (entry.type == "chunked")
              then ^ { chunked = true; len = 0 }
              else ^ result2

-- ABNF for chunked encoding:
--
-- chunked-body   = *chunk
--                  last-chunk
--                  trailer-section
--                  CRLF
--
-- chunk          = chunk-size [ chunk-ext ] CRLF
--                  chunk-data CRLF
-- chunk-size     = 1*HEXDIG
-- last-chunk     = 1*("0") [ chunk-ext ] CRLF
--
-- chunk-data     = 1*OCTET ; a sequence of chunk-size octets
-- chunk-ext      = *( BWS ";" BWS chunk-ext-name
--                     [ BWS "=" BWS chunk-ext-val ] )
--
-- chunk-ext-name = token
-- chunk-ext-val  = token / quoted-string
def HTTP_message_body (ty: HTTP_body_type) =
  First
    -- NOTE: the HTTP specification indicates that we ought to remove
    -- 'chunked' from the Transfer-Encoding list once we parse the
    -- request's chunks. We aren't doing that yet.
    Chunked = block
      ty.chunked is true

      -- content chunks:
      chunks = Many BodyChunk

      -- last-chunk:
      $['0']; CRLF

      -- trailer fields:
      trailer_fields = Many { $$ = HTTP_field_line; CRLF }

      -- Final required CRLF
      CRLF

    Normal = block
      ty.chunked is false
      body = TakeNum ty.len

def TakeNum n =
  block
    let cur = GetStream
    $$ = Take n cur
    SetStream (Drop n cur)

def BodyChunk =
  block
    size = ChunkSize
    extensions = Many ChunkExtension
    CRLF

    -- We forbid zero-sized chunks here because parsing the last chunk
    -- is slightly different and is done above in HTTP_message_body.
    size > 0 is true

    contents = TakeNum size
    CRLF

-- https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
def ChunkExtension =
  block
    HTTP_OWS
    $[';']
    HTTP_OWS
    name = HTTP_token
    value =
      Optional block
        HTTP_OWS
        $['=']
        HTTP_OWS
        First
          Token = HTTP_token
          QuotedString = HTTP_quoted_string

def ChunkSize = HexNumber

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

    -- NOTE: should the following two cases be factored out and used
    -- only when the request method is the appropriate method?
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

--------------------------------------------------------------------------------
-- Field (Header) parsing
--------------------------------------------------------------------------------

def Field_name =
  block
    -- Header names must start with an ASCII letter and can include
    -- letters, digits, or '-' characters.
    let head = CaseInsensitiveAlpha
    let tail = Many
                 First
                   CaseInsensitiveAlpha
                   $['-']
                   $digit

    build (emitArray (emit builder head) tail)

-- Given a parser P and a separator parser Sep, parse and return a
-- sequence of one or more instances of P separated by Sep.
def SepBy1 P Sep =
  block
    let a = many (buf = emit builder P) { Sep; let next = P; emit buf next }
    build a

-- ABNF:
-- Transfer-Encoding = [ transfer-coding *( OWS "," OWS transfer-coding ) ]
def Transfer_Encoding_List =
  SepBy1 Transfer_coding_entry { HTTP_OWS; $[',']; HTTP_OWS }

-- ABNF:
-- transfer-coding    = token *( OWS ";" OWS transfer-parameter )
def Transfer_coding_entry =
  block
    -- Coding names are case-insensitive, despite the ABNF using 'token'
    -- as is used elsewhere:
    -- https://www.rfc-editor.org/rfc/rfc9112#section-7-2
    type = HTTP_token_ci
    params = Many
      block
        HTTP_OWS
        $[';']
        HTTP_OWS
        Transfer_parameter

-- ABNF:
-- transfer-parameter = token BWS "=" BWS ( token / quoted-string )
def Transfer_parameter =
  block
    name = HTTP_token
    HTTP_OWS
    $['=']
    HTTP_OWS
    value = First
      Token = HTTP_token
      QuotedString = HTTP_quoted_string

-- Parse an HTTP header. We specifically parse Content-Length and
-- Transfer-Encoding for use elsewhere in the parser; all other headers
-- are represented as Header { ... }.
def HTTP_field_line =
  block
    let name = Field_name
    $[':']
    HTTP_OWS

    First
      Transfer_Encoding =
        block
          if (name == "transfer-encoding")
            then block
              encodings = Transfer_Encoding_List
            else Fail "Not a transfer-encoding header"

      -- NOTE: the HTTP specification says that there is no upper limit
      -- on the value of Content-Length. We limit it to 64 bits here out
      -- of practicality.
      Content_Length =
        block
          if (name == "content-length")
            then block
              value = PositiveNum64
            else Fail "Not a content-length header"

      Header =
        block
          header_name = name
          let cur = GetStream
          let field_len = HTTP_field_content
          value = Take field_len cur
          SetStream (Drop field_len cur)

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
def HTTP_token_ci = Many (1..) CaseInsensitiveAlpha

-- Utilities to help parse header names case-insensitively while also
-- normalizing them to lowercase so we can check for specific headers in
-- the parser.
def LowerCaseAlpha = $['a' .. 'z']

def UpperCaseAlphaToLower =
  block
    let l = $['A' .. 'Z']
    ^ l + ('a' - 'A')

def CaseInsensitiveAlpha =
  First
    LowerCaseAlpha
    UpperCaseAlphaToLower

def $http_field_vchar = $vchar | $obs_text

def $http_tchar = 
  '!'  | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' |
   '^' | '_' | '`' | '|' | '~' | $digit | $alpha
