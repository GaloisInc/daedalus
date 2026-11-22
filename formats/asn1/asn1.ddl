-- Generic ASN.1 DER parser.
--
-- Parses a single DER-encoded value (identifier, length, contents) into
-- a recursive value tree.  The encoding rules are strict DER:
-- indefinite lengths, non-minimal length/tag/integer/OID encodings,
-- non-canonical booleans, constructed string types, and nonzero
-- bit-string padding are all rejected.
--
-- Deliberately unchecked (see README.md): UTF8String contents are not
-- validated as UTF-8, SET elements are not checked for DER ordering,
-- and time values are not checked for calendar validity.

def Main =
  block
    $$ = Value
    END

----------------------------------------------------------------------
-- Small helpers (defined locally so the spec has no imports)

def Guard (b : bool) = b is true

-- Run P on exactly n bytes of input, then continue after them.
-- Take fails if fewer than n bytes remain (truncated input), and END
-- forces P to consume the whole chunk.
def ChunkExact (n : uint 64) P =
  block
    let s = GetStream
    SetStream (Take n s)
    $$ = P
    END
    SetStream (Drop n s)

-- Check that P accepts the rest of the current chunk, consuming nothing.
def MatchesAhead P =
  block
    let s = GetStream
    P
    END
    SetStream s

-- Last element of a byte array (0 if empty).
def lastByte (xs : [uint 8]) : uint 8 =
  for (acc = (0 : uint 8); x in xs) x

----------------------------------------------------------------------
-- Identifier and length octets

def Header =
  block
    let b       = UInt8
    tagClass    = ^ (b >> 6)             -- 0=universal 1=application 2=context 3=private
    constructed = ^ (((b >> 5) .&. 1) == 1)
    tagNum      = TagNumber (b .&. 0x1F)
    len         = Length

def TagNumber (low : uint 8) =
  if low < 0x1F
    then ^ (low as uint 64)
    else
      -- High tag-number form: base-128, minimal encoding required.
      block
        let first = UInt8
        Guard (first != 0x80)            -- no leading zero septet
        let v = Base128Rest ((first .&. 0x7F) as uint 64) ((first .&. 0x80) != 0)
        Guard (v >= 0x1F)                -- must not fit the low form
        ^ v

-- Continue a base-128 (7 bits per octet, high bit = more) quantity.
-- Also used for OID subidentifiers.
def Base128Rest (acc : uint 64) (more : bool) =
  if more
    then
      block
        Guard (acc <= 0x01FFFFFFFFFFFFFF)  -- room for 7 more bits
        let b = UInt8
        Base128Rest ((acc << 7) .|. ((b .&. 0x7F) as uint 64)) ((b .&. 0x80) != 0)
    else ^ acc

def Length =
  block
    let b = UInt8
    if b < 0x80
      then ^ (b as uint 64)              -- short form
      else LongLength ((b .&. 0x7F) as uint 64)

def LongLength (n : uint 64) =
  block
    Guard (n >= 1)                       -- 0x80 is indefinite length: not DER
    Guard (n <= 8)                       -- fits uint 64; also rejects reserved 0xFF
    let first = UInt8
    Guard (first != 0)                   -- minimal: no leading zero octet
    let rest = Many (n - 1) UInt8
    let len  = for (acc = (first as uint 64); x in rest)
                 ((acc << 8) .|. (x as uint 64))
    Guard (len >= 0x80)                  -- minimal: must not fit the short form
    ^ len

----------------------------------------------------------------------
-- Values (recursive through SEQUENCE/SET and constructed non-universal tags)

def Value =
  block
    let hdr = Header
    ChunkExact hdr.len (Content hdr)

def Content hdr =
  case hdr.tagClass of
    0 -> UniversalContent hdr
    _ -> {| other = OtherValue hdr |}

def UniversalContent hdr =
  case hdr.tagNum of
    1  -> {| boolean         = Boolean hdr |}
    2  -> {| integer         = IntegerBytes hdr |}
    3  -> {| bitString       = BitString hdr |}
    4  -> {| octetString     = RawBytes hdr |}
    5  -> {| null            = Null hdr |}
    6  -> {| oid             = ObjectId hdr |}
    12 -> {| utf8String      = RawBytes hdr |}
    16 -> {| sequence        = ConstructedValue hdr |}
    17 -> {| set             = ConstructedValue hdr |}
    19 -> {| printableString = PrintableStr hdr |}
    22 -> {| ia5String       = Ia5Str hdr |}
    23 -> {| utcTime         = UtcTime hdr |}
    24 -> {| generalizedTime = GenTime hdr |}
    _  -> {| other           = OtherValue hdr |}  -- e.g. ENUMERATED, REAL

def Boolean hdr =
  block
    hdr.constructed is false
    let b = UInt8                        -- length == 1 enforced by ChunkExact's END
    case b of                            -- DER: only 0x00 / 0xFF
      0x00 -> ^ false
      0xFF -> ^ true

-- INTEGER (and would-be ENUMERATED) content is kept as raw big-endian
-- two's-complement bytes, so arbitrarily large values are supported.
def IntegerBytes hdr =
  block
    hdr.constructed is false
    let first = UInt8                    -- at least one octet required
    let rest  = Many UInt8
    Guard (minimalInt first rest)
    ^ concat [[first], rest]

-- DER: the first 9 bits may not be all zeros or all ones.
def minimalInt (first : uint 8) (rest : [uint 8]) : bool =
  for (ok = true; i, b in rest)
    (if i == 0
       then !(   (first == 0x00 && (b .&. 0x80) == 0x00)
              || (first == 0xFF && (b .&. 0x80) == 0x80))
       else ok)

def BitString hdr =
  block
    hdr.constructed is false             -- DER: primitive only
    unused = UInt8
    bytes  = Many UInt8
    Guard (unused <= 7)
    Guard (length bytes > 0 || unused == 0)
    Guard (length bytes == 0 || ((lastByte bytes) .&. (lowMask unused)) == 0)

def lowMask (n : uint 8) : uint 8 =
  ((1 : uint 8) << (n as uint 64)) - 1

def Null hdr =
  block
    hdr.constructed is false
    ^ {}                                 -- empty content enforced by ChunkExact's END

def ObjectId hdr =
  block
    hdr.constructed is false
    let first = SubIdentifier            -- at least one subidentifier required
    let a0    = firstArc first
    let rest  = Many SubIdentifier
    ^ concat [[a0, first - a0 * 40], rest]

def firstArc (n : uint 64) : uint 64 =
  if n < 40 then 0 else (if n < 80 then 1 else 2)

def SubIdentifier =
  block
    let first = UInt8
    Guard (first != 0x80)                -- minimal encoding
    Base128Rest ((first .&. 0x7F) as uint 64) ((first .&. 0x80) != 0)

def RawBytes hdr =
  block
    hdr.constructed is false
    $$ = Many UInt8

-- NOTE: the character class is inlined (instead of a named `def $printable`)
-- because named character classes are not supported by compile-rust.
def PrintableStr hdr =
  block
    hdr.constructed is false
    $$ = Many $[ 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
               | ' ' | 0x27 | '(' | ')' | '+' | ',' | '-' | '.'
               | '/' | ':' | '=' | '?' ]

def Ia5Str hdr =
  block
    hdr.constructed is false
    $$ = Many $[0x00 .. 0x7F]

-- Times are validated for shape, but returned as their raw bytes.

def UtcTime hdr =
  block
    hdr.constructed is false
    MatchesAhead UtcTimeBody
    $$ = Many UInt8

def UtcTimeBody =
  block
    Many 12 $['0' .. '9']                -- DER: exactly YYMMDDHHMMSSZ
    $['Z']

def GenTime hdr =
  block
    hdr.constructed is false
    MatchesAhead GenTimeBody
    $$ = Many UInt8

def GenTimeBody =
  block
    Many 14 $['0' .. '9']                -- DER: YYYYMMDDHHMMSS[.f+]Z
    Optional Fraction
    $['Z']

def Fraction =
  block
    $['.']
    let ds = Many (1..) $['0' .. '9']
    Guard (lastByte ds != '0')           -- DER: no trailing zero in the fraction

def ConstructedValue hdr =
  block
    hdr.constructed is true
    $$ = Many Value

-- Application, context-specific, and private class tags, plus
-- unsupported universal tags: keep the header info, and recurse if
-- constructed.
def OtherValue hdr =
  block
    tagClass    = ^ hdr.tagClass
    constructed = ^ hdr.constructed
    tagNum      = ^ hdr.tagNum
    content     = OtherContent hdr.constructed

def OtherContent (constructed : bool) =
  case constructed of
    true  -> {| children = Many Value |}
    false -> {| raw = Many UInt8 |}
