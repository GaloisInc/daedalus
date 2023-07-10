-- HTTP 1.1 message parser
--
-- Primary reference: https://www.rfc-editor.org/rfc/rfc9112
--
-- Specific section and paragraph links to RFC 9112 are provided below
-- where appropriate.

import Utils
import Lexemes
import URI

--------------------------------------------------------------------------------
-- Entry points
--------------------------------------------------------------------------------

-- Request parser
def HTTP_request = HTTP_message false HTTP_request_line

-- Response parser
def HTTP_status = HTTP_message true HTTP_status_line

--------------------------------------------------------------------------------

-- Parse an HTTP message.
--
-- is_response is true if a response is being parsed, or false if not.
-- StartLine is the parser for the first line up to (but not including)
-- the first CRLF.
--
-- Note: we do not use END at the end of this parser both to ease
-- testing in development and also to make this parser more cooperative
-- with calling parsers which may want to decide what to do with
-- leftover input.
def HTTP_message is_response StartLine =
  block
    start = StartLine
    CRLF
    field_info = HTTP_field_info
    CRLF
    let ty = HTTP_body_type is_response field_info
    body = HTTP_message_body ty

-- The types of HTTP message bodies.
def HTTP_body_type_u =
  union
    -- The body is transfer-encoded 'chunked'
    ty_chunked: { }
    -- The body is a byte sequence of the explicitly-specified length
    ty_normal_len: uint 64
    -- The body is of indeterminate length and should be consumed until
    -- the connection is closed (i.e. until the input is exhausted)
    ty_read_all: { }

-- Parse HTTP fields. Also extract information about content-length (in
-- the 'explicit_length' field), chunked encoding (in the 'chunked'
-- field), and the presence of any other transfer-encoding (in the
-- 'encoded' field). Note that this only records these facts as they are
-- found in the fields; precedence or other considerations related to
-- these facts are expressed in HTTP_body_type.
--
-- The spec does not give any guidance about how to handle duplicates of
-- these fields. This parser respects the last occurrence of each of the
-- Content-Length and Transfer-Encoding fields. The original fields are
-- preserved in order with possible duplication in the 'fields' field.
def HTTP_field_info =
  block
    fields = Many { $$ = HTTP_field; CRLF }

    let result = for (result = { chunked = false, encoded = false, len = nothing }; f in fields)
      case f: HTTP_field_u of
        Field _ -> ^ result

        Content_Length l ->
          ^ { chunked = result.chunked, encoded = result.encoded, len = just l }

        Transfer_Encoding h ->
          ^ { chunked = h.is_chunked, encoded = true, len = result.len }

    chunked = result.chunked
    explicit_length = result.len
    encoded = result.encoded

-- Determine the message body type from the message's fields.
--
-- If the Transfer-Encoding field is present and includes 'chunked'
-- as its last entry, the body type is chunked and should be parsed
-- accordingly. If the body is not chunked but has some other encoding,
-- the body length is indeterminate and should be consumed in its
-- entirety. If the Transfer-Encoding field is absent and the
-- Content-Length field is present, then the body should be treated as
-- an octet sequence of length specified by Content-Length. Otherwise
-- the body is of indeterminate length.
--
-- is_response should be true if the body is a reponse message body.
-- For responses, a missing explicitly-specified Content-Length
-- and a missing last 'chunked' entry in the Transfer-Encoding
-- encoding list indicates that the length is obtained by
-- receiving all octets until the connection is closed. (See
-- https://www.rfc-editor.org/rfc/rfc9112#section-6.3-2.8)
def HTTP_body_type is_response (field_info: HTTP_field_info): HTTP_body_type_u =
  block
    ^ if field_info.chunked
        then {| ty_chunked |}
        else if field_info.encoded
          then {| ty_read_all |}
          else case field_info.explicit_length of
            nothing -> if is_response
                         -- Responses are of indeterminate length in this case
                         then {| ty_read_all |}
                         -- But requests are zero-length in this case
                         else {| ty_normal_len = 0 |}
            just l -> {| ty_normal_len = l |}

def HTTP_message_chunked_s =
  struct
    chunks: [BodyChunk]
    trailer_fields: [HTTP_field_u]

def HTTP_message_body_u =
  union
    -- The chunks parsed from the body.
    chunked: HTTP_message_chunked_s

    -- The array of bytes containing the body.
    bytes: [uint 8]

    -- All remaining bytes in the input.
    remaining: [uint 8]

-- Parse an HTTP message body based on the specified body type. See
-- BodyChunk for relevant ABNF.
def HTTP_message_body (ty: HTTP_body_type_u): HTTP_message_body_u =
  case ty of
    ty_chunked ->
      block
        -- content chunks:
        let chunks = Many BodyChunk

        -- last-chunk:
        Many (1..) $['0']; CRLF

        -- trailer fields:
        let trailer_fields = Many { $$ = HTTP_field; CRLF }

        -- Final required CRLF
        CRLF

        ^ {| chunked = { chunks = chunks, trailer_fields = trailer_fields } |}

    ty_normal_len len ->
      block
        let body = Many len $any
        ^ {| bytes = body |}

    ty_read_all ->
      block
        let bs = Many $any
        ^ {| remaining = bs |}

-- Parse a single chunk in a message body that has Transfer-Encoding:
-- chunked.
--
-- Relevant ABNF for chunked encoding:
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
def BodyChunk =
  block
    size = ChunkSize
    extensions = Many ChunkExtension
    CRLF

    -- We forbid zero-sized chunks here because parsing the last chunk
    -- is slightly different and is done above in HTTP_message_body. The
    -- zero-sized last chunk also ends in a CRLF, but the difference
    -- between a zero-sized last chunk and a preceding chunk is that the
    -- last chunk has an optional list of headers in between the size
    -- and the CRLF.
    size > 0 is true

    contents = Many size $any
    CRLF

-- Parse a chunk extension.
--
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

-- Parse a chunked body chunk size.
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
    status_code = StatusCode
    $sp
    reason = Many $[ $htab | $sp | $vchar | $obs_text ]

def HTTP_request_line =
  block
    method = HTTP_method
    $sp
    target = HTTP_request_target method
    $sp
    version = HTTP_version

def HTTP_request_target (method : HTTP_method) =
  case method of
    CONNECT -> {| Authority = HTTP_authority_form |}
    _ ->
      First
        AbsoluteURI = URI_absolute_URI        -- old style and for proxies
        Origin      = HTTP_origin_form        -- normal request
        Asterisk    = { method is OPTIONS; @$['*'] }

def HTTP_authority_form =
  block
    host  = URI_host
    $[':']
    port  = URI_port

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
    -- Field names must start with an ASCII letter and can include
    -- letters, digits, or '-' characters.
    let head = $alpha
    let tail = Many $[ $alpha | '-' | $digit ]
    let result = concat [[head], tail]
    ^ map (c in result)
        toLower c

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
    -- as is used elsewhere, so we use the case-insensitive normalizing
    -- token parser here instead.
    --
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

def Transfer_Encoding_field_s =
  struct
    is_chunked: bool
    encodings: [Transfer_coding_entry]

def Header_s =
  struct
    name: [uint 8]
    value: [uint 8]

def HTTP_field_u =
  union
    Transfer_Encoding: Transfer_Encoding_field_s
    Content_Length: uint 64
    Field: Header_s

-- Parse an HTTP field (header). We specifically parse Content-Length
-- and Transfer-Encoding for use elsewhere in the parser; all other
-- headers are represented as Field { ... }.
def HTTP_field: HTTP_field_u =
  block
    let field_name = Field_name
    $[':']
    HTTP_OWS

    case field_name of
      "transfer-encoding" ->
        block
          let encodings = Transfer_Encoding_List
          let last = Last encodings
          let chunked = last.type == "chunked"

          -- The spec says we should only treat the body as chunked if
          -- 'chunked' is last in the encoding list. Remove it from the
          -- encoding list since we're going to decode the chunks.
          -- References:
          -- https://www.rfc-editor.org/rfc/rfc9112#section-6.3-2.4.1
          -- https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1-3
          -- https://www.rfc-editor.org/rfc/rfc9112#section-7.1.2-3
          let init_encodings = if chunked
                                 then Init encodings
                                 else encodings

          ^ {| Transfer_Encoding = { is_chunked = chunked,
                                     encodings = init_encodings }
             |}

      "content-length" ->
         -- NOTE: the HTTP specification says that there is no upper
         -- limit on the value of Content-Length. We limit it to 64 bits
         -- here out of practicality.
         --
         -- NOTE: the specification permits all field values to be
         -- either tokens or quoted strings. This handles both. The
         -- specification also states that a Content-Length field is
         -- valid if it is a comma-separated list of numbers, all of
         -- which take the same value. We also handle that case here.
         --
         -- https://www.rfc-editor.org/rfc/rfc9110#section-5.5-12
         -- https://www.rfc-editor.org/rfc/rfc9112#section-6.3-2.5
         block
           let values = SepBy1 (MaybeQuoted PositiveNum64) { HTTP_OWS; $[',']; HTTP_OWS }
           let first = Head values
           let checked = for (result = first; val in values)
                           block
                             (val == first) is true
                             ^ first
           ^ {| Content_Length = checked |}

      _ ->
        block
          let cur = GetStream
          let field_len = HTTP_field_content
          let value = bytesOfStream (Take field_len cur)
          SetStream (Drop field_len cur)
          ^ {| Field = { name = field_name, value = value } |}

-- Get the last element of a list.
def Last (a: [?a]): ?a =
  Index a (length a - 1)

-- Get the first element of a list.
def Head (a: [?a]): ?a =
  Index a 0

-- Get all but the last element of a list.
def Init (a: [?a]): [?a] =
  block
    let l = length a
    let result = for (b = builder; i, e in a)
      block
        if i < l - 1
          then emit b e
          else b
    build result

def HTTP_field_content =
  many (count = 0)
    block
      let n = HTTP_OWS
      $http_field_vchar
      count + n + 1

--------------------------------------------------------------------------------
-- Field Values
--------------------------------------------------------------------------------

def Quoted P = { $dquote; $$ = P; $dquote }

def HTTP_quoted_string = Quoted HTTP_string

def HTTP_string =
  Many ( $[$htab | $sp | 0x21 | 0x23 .. 0x5B | 0x5D .. 0x7E | $obs_text]
      <| HTTP_quoted_pair
       )

def MaybeQuoted P =
  First
    Quoted P
    P

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

-- Token parsers
def HTTP_token = Many (1..) $http_tchar

-- Parse a token, but normalize its alphabetic characters to lowercase.
def HTTP_token_ci =
  Many (1..)
    First
      $http_tchar_noalpha
      toLower $alpha

def StatusCode =
  block
    let d1 = DigitNum
    let d2 = DigitNum
    let d3 = DigitNum
    ^ (d1 * 100 + d2 * 10 + d3)

def toLower c =
  if c >= 'A' && c <= 'Z'
    then c + ('a' - 'A')
    else c

def $http_field_vchar = $vchar | $obs_text

def $http_tchar = $http_tchar_noalpha | $alpha

def $http_tchar_noalpha =
  '!'  | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' |
   '^' | '_' | '`' | '|' | '~' | $digit
