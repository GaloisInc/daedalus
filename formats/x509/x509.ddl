-- X.509 certificate parser (RFC 5280, DER encoding).
--
-- Schema-directed: parses a Certificate into named fields
-- (serial number, issuer, validity, subject public key info, decoded
-- standard extensions, ...), enforcing both the certificate structure
-- and strict DER encoding rules along the way.  ANY slots (algorithm
-- parameters, attribute values, unknown extensions) fall back to a
-- generic ASN.1 value tree.
--
-- The DER core (header/length/content primitives and the generic Any
-- value) is shared with the sibling example in formats/asn1; the
-- examples are kept self-contained, so the code is duplicated rather
-- than imported.
--
-- Enforced beyond basic DER: version is 1..2 when present ([0] absent
-- means v1, and encoding the DEFAULT 0 is a DER violation), unique IDs
-- require v2+, extensions require v3, `critical` and `cA` booleans may
-- not be encoded as their DEFAULT FALSE, SIZE (1..MAX) bounds on
-- extension/RDN/GeneralName sequences.
--
-- Not checked (see README.md): signatures, serial number positivity,
-- SET ordering, UTF-8 validity, calendar validity.

def Main =
  block
    $$ = Certificate
    END

----------------------------------------------------------------------
-- Certificate structure (RFC 5280, section 4.1)

def Certificate = Sequence CertificateBody

def CertificateBody =
  block
    tbsCertificate     = TBSCertificate
    signatureAlgorithm = AlgorithmIdentifier
    signatureValue     = BitStringField

def TBSCertificate = Sequence TBSBody

def TBSBody =
  block
    version              = VersionField            -- 0..2; absent => 0 (v1)
    serialNumber         = IntegerField            -- raw content bytes
    signature            = AlgorithmIdentifier
    issuer               = Name
    validity             = Validity
    subject              = Name
    subjectPublicKeyInfo = SubjectPublicKeyInfo
    issuerUniqueID       = OptUniqueId version 0x81 1
    subjectUniqueID      = OptUniqueId version 0x82 2
    extensions           = OptExtensions version

-- version [0] EXPLICIT INTEGER DEFAULT v1(0)
def VersionField =
  block
    let b = PeekByte
    if presentIf b 0xA0
      then Tagged 2 0 true ExplicitVersion
      else ^ 0

def ExplicitVersion = Tagged 0 2 false VersionNumber

def VersionNumber =
  block
    let v = UInt8
    -- 0 would encode the DEFAULT (DER violation); > 2 is out of range.
    Guard (v == 1 || v == 2)
    ^ (v as uint 64)

-- issuerUniqueID [1] / subjectUniqueID [2] IMPLICIT BIT STRING OPTIONAL
def OptUniqueId (ver : uint 64) (idOctet : uint 8) (tagNum : uint 64) =
  block
    let b = PeekByte
    if presentIf b idOctet
      then UniqueIdPresent ver tagNum
      else ^ nothing

def UniqueIdPresent (ver : uint 64) (tagNum : uint 64) =
  block
    Guard (ver >= 1)                   -- unique IDs require v2 or v3
    JustOf (Tagged 2 tagNum false BitStringBody)

-- extensions [3] EXPLICIT SEQUENCE SIZE (1..MAX) OF Extension OPTIONAL
def OptExtensions (ver : uint 64) =
  block
    let b = PeekByte
    if presentIf b 0xA3
      then ExtensionsPresent ver
      else ^ nothing

def ExtensionsPresent (ver : uint 64) =
  block
    Guard (ver == 2)                   -- extensions require v3
    JustOf (Tagged 2 3 true ExtensionsSeq)

def ExtensionsSeq = Sequence (Many (1..) Extension)

def AlgorithmIdentifier = Sequence AlgorithmIdentifierBody

def AlgorithmIdentifierBody =
  block
    algorithm  = OIDField
    parameters = IfMore Any            -- e.g. NULL for RSA, curve OID for EC

def Name = Sequence (Many RDN)

def RDN = SetOf (Many (1..) ATV)       -- SET SIZE (1..MAX)

def ATV = Sequence ATVBody

-- `type`/`value` in RFC 5280; renamed because `type` is a Rust keyword
-- and would clash in the generated code.
def ATVBody =
  block
    attrType  = OIDField
    attrValue = Any

def Validity = Sequence ValidityBody

def ValidityBody =
  block
    notBefore = Time
    notAfter  = Time

def Time =
  block
    let hdr = Header
    ChunkExact hdr.len (TimeContent hdr)

def TimeContent hdr =
  block
    Guard (hdr.tagClass == 0 && !hdr.constructed)
    case hdr.tagNum of
      23 -> {| utcTime         = CheckedUtcTime |}
      24 -> {| generalizedTime = CheckedGenTime |}

def CheckedUtcTime =
  block
    MatchesAhead UtcTimeShape
    $$ = Many UInt8

def CheckedGenTime =
  block
    MatchesAhead GenTimeShape
    $$ = Many UInt8

def SubjectPublicKeyInfo = Sequence SPKIBody

def SPKIBody =
  block
    algorithm        = AlgorithmIdentifier
    subjectPublicKey = BitStringField

----------------------------------------------------------------------
-- Extensions (RFC 5280, section 4.2)

def Extension = Sequence ExtensionBody

def ExtensionBody =
  block
    extnID    = OIDField
    critical  = OptCritical
    extnValue = Tagged 0 4 false (ExtnContent extnID)

-- critical BOOLEAN DEFAULT FALSE: if present it must be TRUE, because
-- DER forbids encoding a DEFAULT value.
def OptCritical =
  block
    let b = PeekByte
    if presentIf b 0x01
      then TrueBooleanField
      else ^ false

def TrueBooleanField =
  block
    let v = BooleanField
    v is true
    ^ true

-- The OCTET STRING content of extnValue is itself DER; decode the
-- standard extensions, keep everything else raw.
def ExtnContent (oid : [uint 64]) =
  if oid == [2, 5, 29, 19]      then {| basicConstraints = BasicConstraints |}
  else if oid == [2, 5, 29, 15] then {| keyUsage         = BitStringField |}
  else if oid == [2, 5, 29, 17] then {| subjectAltName   = GeneralNames |}
  else if oid == [2, 5, 29, 14] then {| subjectKeyId     = OctetStringField |}
  else if oid == [2, 5, 29, 35] then {| authorityKeyId   = AuthorityKeyId |}
  else if oid == [2, 5, 29, 37] then {| extKeyUsage      = ExtKeyUsage |}
  else                               {| raw              = Many UInt8 |}

-- BasicConstraints ::= SEQUENCE { cA BOOLEAN DEFAULT FALSE,
--                                 pathLenConstraint INTEGER OPTIONAL }
def BasicConstraints = Sequence BasicConstraintsBody

def BasicConstraintsBody =
  block
    ca      = OptCA
    pathLen = OptPathLen

def OptCA =
  block
    let b = PeekByte
    if presentIf b 0x01
      then TrueBooleanField            -- FALSE would encode the DEFAULT
      else ^ false

def OptPathLen =
  block
    let b = PeekByte
    if presentIf b 0x02
      then JustOf SmallIntField
      else ^ nothing

-- An INTEGER that must fit in uint 64 and be non-negative.
def SmallIntField = Tagged 0 2 false SmallIntBody

def SmallIntBody =
  block
    let bytes = IntegerBody
    let first = Index bytes 0
    Guard ((first .&. 0x80) == 0)
    Guard (length bytes <= 8)
    ^ for (acc = (0 : uint 64); x in bytes) ((acc << 8) .|. (x as uint 64))

-- SubjectAltName ::= GeneralNames ::= SEQUENCE SIZE (1..MAX) OF GeneralName
def GeneralNames = Sequence (Many (1..) GeneralName)

def GeneralName =
  block
    let hdr = Header
    Guard (hdr.tagClass == 2)          -- all GeneralName forms are context-tagged
    ChunkExact hdr.len (GeneralNameContent hdr)

def GeneralNameContent hdr =
  case hdr.tagNum of
    1 -> {| rfc822Name    = Ia5Bytes hdr |}
    2 -> {| dnsName       = Ia5Bytes hdr |}
    4 -> {| directoryName = DirName hdr |}
    6 -> {| uri           = Ia5Bytes hdr |}
    7 -> {| ipAddress     = PrimBytes hdr |}
    _ -> {| otherForm     = GeneralNameOther hdr |}

def Ia5Bytes hdr =
  block
    hdr.constructed is false
    $$ = Many $[0x00 .. 0x7F]

def PrimBytes hdr =
  block
    hdr.constructed is false
    $$ = Many UInt8

-- directoryName [4] wraps a Name (EXPLICIT, since Name is a CHOICE).
def DirName hdr =
  block
    hdr.constructed is true
    $$ = Name

def GeneralNameOther hdr =
  block
    tagNum      = ^ hdr.tagNum
    constructed = ^ hdr.constructed
    content     = AnyChildren hdr.constructed

-- AuthorityKeyIdentifier ::= SEQUENCE {
--   keyIdentifier [0] IMPLICIT OCTET STRING OPTIONAL, ... }
-- The optional authorityCertIssuer/serial tail is kept raw.
def AuthorityKeyId = Sequence AuthorityKeyIdBody

def AuthorityKeyIdBody =
  block
    keyId = OptKeyId
    rest  = Many UInt8

def OptKeyId =
  block
    let b = PeekByte
    if presentIf b 0x80
      then JustOf (Tagged 2 0 false ManyBytes)
      else ^ nothing

def ExtKeyUsage = Sequence (Many (1..) OIDField)

----------------------------------------------------------------------
-- Schema combinators

-- Expect an exact header, then run P on exactly the content bytes.
def Tagged (cls : uint 8) (num : uint 64) (cons : bool) P =
  block
    let hdr = Header
    Guard (hdr.tagClass == cls && hdr.tagNum == num && hdr.constructed == cons)
    ChunkExact hdr.len P

def Sequence P = Tagged 0 16 true P
def SetOf P    = Tagged 0 17 true P

def BooleanField     = Tagged 0 1 false BooleanBody
def IntegerField     = Tagged 0 2 false IntegerBody
def BitStringField   = Tagged 0 3 false BitStringBody
def OctetStringField = Tagged 0 4 false ManyBytes
def OIDField         = Tagged 0 6 false ObjectIdBody

def ManyBytes = Many UInt8

-- Look at the next byte without consuming it (nothing at end of input).
def PeekByte =
  block
    let s = GetStream
    $$ = Optional UInt8
    SetStream s

def presentIf (b : maybe (uint 8)) (idOctet : uint 8) : bool =
  case b of
    just x  -> x == idOctet
    nothing -> false

def JustOf P =
  block
    let v = P
    ^ just v

-- Parse P if any input remains in the current chunk, else nothing.
def IfMore P =
  block
    let b = PeekByte
    case b of
      just x  -> JustOf P
      nothing -> ^ nothing

----------------------------------------------------------------------
-- DER core: identifier and length octets (shared with formats/asn1)

def Guard (b : bool) = b is true

-- Run P on exactly n bytes of input, then continue after them.
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

def lastByte (xs : [uint 8]) : uint 8 =
  for (acc = (0 : uint 8); x in xs) x

def Header =
  block
    let b       = UInt8
    tagClass    = ^ (b >> 6)
    constructed = ^ (((b >> 5) .&. 1) == 1)
    tagNum      = TagNumber (b .&. 0x1F)
    len         = Length

def TagNumber (low : uint 8) =
  if low < 0x1F
    then ^ (low as uint 64)
    else
      block
        let first = UInt8
        Guard (first != 0x80)
        let v = Base128Rest ((first .&. 0x7F) as uint 64) ((first .&. 0x80) != 0)
        Guard (v >= 0x1F)
        ^ v

def Base128Rest (acc : uint 64) (more : bool) =
  if more
    then
      block
        Guard (acc <= 0x01FFFFFFFFFFFFFF)
        let b = UInt8
        Base128Rest ((acc << 7) .|. ((b .&. 0x7F) as uint 64)) ((b .&. 0x80) != 0)
    else ^ acc

def Length =
  block
    let b = UInt8
    if b < 0x80
      then ^ (b as uint 64)
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
-- DER core: content bodies (constructedness checked by the caller)

def BooleanBody =
  block
    let b = UInt8
    case b of                            -- DER: only 0x00 / 0xFF
      0x00 -> ^ false
      0xFF -> ^ true

-- Raw big-endian two's-complement bytes; minimality enforced.
def IntegerBody =
  block
    let first = UInt8
    let rest  = Many UInt8
    Guard (minimalInt first rest)
    ^ concat [[first], rest]

def minimalInt (first : uint 8) (rest : [uint 8]) : bool =
  for (ok = true; i, b in rest)
    (if i == 0
       then !(   (first == 0x00 && (b .&. 0x80) == 0x00)
              || (first == 0xFF && (b .&. 0x80) == 0x80))
       else ok)

def BitStringBody =
  block
    unused = UInt8
    bytes  = Many UInt8
    Guard (unused <= 7)
    Guard (length bytes > 0 || unused == 0)
    Guard (length bytes == 0 || ((lastByte bytes) .&. (lowMask unused)) == 0)

def lowMask (n : uint 8) : uint 8 =
  ((1 : uint 8) << (n as uint 64)) - 1

def ObjectIdBody =
  block
    let first = SubIdentifier
    let a0    = firstArc first
    let rest  = Many SubIdentifier
    ^ concat [[a0, first - a0 * 40], rest]

def firstArc (n : uint 64) : uint 64 =
  if n < 40 then 0 else (if n < 80 then 1 else 2)

def SubIdentifier =
  block
    let first = UInt8
    Guard (first != 0x80)
    Base128Rest ((first .&. 0x7F) as uint 64) ((first .&. 0x80) != 0)

def UtcTimeShape =
  block
    Many 12 $['0' .. '9']                -- DER: exactly YYMMDDHHMMSSZ
    $['Z']

def GenTimeShape =
  block
    Many 14 $['0' .. '9']                -- DER: YYYYMMDDHHMMSS[.f+]Z
    Optional FractionShape
    $['Z']

def FractionShape =
  block
    $['.']
    let ds = Many (1..) $['0' .. '9']
    Guard (lastByte ds != '0')           -- DER: no trailing zero in the fraction

----------------------------------------------------------------------
-- Generic ASN.1 value for ANY slots (shared with formats/asn1)

def Any =
  block
    let hdr = Header
    ChunkExact hdr.len (AnyContent hdr)

def AnyContent hdr =
  case hdr.tagClass of
    0 -> AnyUniversal hdr
    _ -> {| other = AnyOther hdr |}

def AnyUniversal hdr =
  case hdr.tagNum of
    1  -> {| boolean         = AnyBoolean hdr |}
    2  -> {| integer         = AnyInteger hdr |}
    3  -> {| bitString       = AnyBitString hdr |}
    4  -> {| octetString     = AnyRawBytes hdr |}
    5  -> {| null            = AnyNull hdr |}
    6  -> {| oid             = AnyObjectId hdr |}
    12 -> {| utf8String      = AnyRawBytes hdr |}
    16 -> {| sequence        = AnySeq hdr |}
    17 -> {| set             = AnySet hdr |}
    19 -> {| printableString = AnyPrintable hdr |}
    22 -> {| ia5String       = AnyIa5 hdr |}
    23 -> {| utcTime         = AnyUtcTime hdr |}
    24 -> {| generalizedTime = AnyGenTime hdr |}
    _  -> {| other           = AnyOther hdr |}

def AnyBoolean hdr =
  block
    hdr.constructed is false
    BooleanBody

def AnyInteger hdr =
  block
    hdr.constructed is false
    IntegerBody

def AnyBitString hdr =
  block
    hdr.constructed is false
    BitStringBody

def AnyRawBytes hdr =
  block
    hdr.constructed is false
    $$ = Many UInt8

def AnyNull hdr =
  block
    hdr.constructed is false
    ^ {}

def AnyObjectId hdr =
  block
    hdr.constructed is false
    ObjectIdBody

def AnySeq hdr =
  block
    hdr.constructed is true
    $$ = Many Any

def AnySet hdr =
  block
    hdr.constructed is true
    $$ = Many Any

-- NOTE: the character class is inlined (instead of a named `def $printable`)
-- because named character classes are not supported by compile-rust.
def AnyPrintable hdr =
  block
    hdr.constructed is false
    $$ = Many $[ 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
               | ' ' | 0x27 | '(' | ')' | '+' | ',' | '-' | '.'
               | '/' | ':' | '=' | '?' ]

def AnyIa5 hdr =
  block
    hdr.constructed is false
    $$ = Many $[0x00 .. 0x7F]

def AnyUtcTime hdr =
  block
    hdr.constructed is false
    MatchesAhead UtcTimeShape
    $$ = Many UInt8

def AnyGenTime hdr =
  block
    hdr.constructed is false
    MatchesAhead GenTimeShape
    $$ = Many UInt8

def AnyOther hdr =
  block
    tagClass    = ^ hdr.tagClass
    constructed = ^ hdr.constructed
    tagNum      = ^ hdr.tagNum
    content     = AnyChildren hdr.constructed

def AnyChildren (constructed : bool) =
  case constructed of
    true  -> {| children = Many Any |}
    false -> {| raw = Many UInt8 |}
