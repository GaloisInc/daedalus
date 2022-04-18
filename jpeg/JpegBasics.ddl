-- Reference: https://www.w3.org/Graphics/JPEG/itu-t81.pdf
import Stdlib

-- DBG:
def SomeJpeg =
  block
    SOI
    $$ = Many Segment
    EOI

-- Start / End image
def SOI = Marker 0xD8 <| Fail "Missing Start-of-Image"
def EOI = Marker 0xD9 <| Fail "Missing End-of-Image"

-- Generic JPEG segment
-- DBG:
def Segment = 
  block
    segmentStart = Offset
    data         = SegmentBody
    segmentEnd   = Offset

def SegmentBody =
  Choose1
    comment = COM
    dri     = DRI
    sof     = SomeSOF
    sos     = SOS
    app     = SomeAPP
    dqt     = DQT
    dht     = DHT
    rst     = SomeRST
    extra   = @(UInt8 (!0xFF)) -- unexpected data




-- Specific marker
def Marker x = Match [ 0xFF, x ]

-- For families of markers, such as APP or SOF
def SomeMarker (front : uint 4) =
  block
    UInt8 0xFF
    let tag   = UInt8
    let upper = truncate8to4 (tag >> 4)
    upper == front is true
    truncate8to4 tag

-- Segement payload
def Payload P =
  block
    let size = BE16 as uint 64
    size >= 2 is true
    let len = size - 2
    let here = GetStream
    SetStream (Take len here)
    res = P
    END
    SetStream (Drop len here)

-- Comment
def COM =
  block
    Marker 0xFE
    Payload (Many UInt8)

-- Application specific
def APP (x : uint 4) P =
  block
    Marker (0xE # x)
    Payload P

-- Application specific, uninterpreted
def SomeAPP = Choose1
  jfif  = JFIF_APP0
  exif  = Exif_APP1
  other = UnknownApp

def UnknownApp =
  block
    app  = SomeMarker 0xE
    data = Payload (Many UInt8)

-- Start of frame (x /= 4)
def SOF (x : uint 4) =
  block
    Marker (0xC # x)
    SOFPayload

-- Start of frame, uninterpreted
def SomeSOF =
  block
    sof  = SomeMarker 0xC
    sof == 4 is false
    data = SOFPayload

def SOFPayload =
  Payload
  block
    samplePrecision        = UInt8
    numberOfLines          = BE16
    numberOfSamplesPerLine = BE16
    let comNumber          = UInt8 as uint 64
    components             = Many comNumber FrameComponent


def FrameComponent =
  block
    identifier          = UInt8
    let samplingFactors = UInt8
    hoizontalSampling   = truncate8to4 (samplingFactors >> 4)
    verticalSampling    = truncate8to4 samplingFactors
    quantTableSel       = UInt8


-- Start of Scan
def SOS =
  block
    Marker 0xDA
    header = Payload SOSHeader
    data   = Many EntropyEncodedEntry

def SOSHeader =
  block
    let componentNum = UInt8
    components = Many (componentNum as uint 64) SOSComponent
    ss = UInt8
    se = UInt8
    let a = UInt8
    ah = truncate8to4 (a >> 4)
    al = truncate8to4 a

def SOSComponent =
  block
    id      = UInt8
    let table  = UInt8
    acTable = truncate8to4 table
    dcTable = truncate8to4 (table >> 4)


-- Define Huffman tables
def DHT =
  block
    Marker 0xC4
    Payload (Many HT)

-- Huffman table
def HT =
  block
    let info    = UInt8
    ht_class    = truncate8to4 info
    ht_type     = truncate8to4 (info >> 4)
    let symNums = Many 16 UInt8
    table       = map (n in symNums) (Many (n as uint 64) UInt8)

-- Define quantization tables
def DQT =
  block
    Marker 0xDB
    Payload (Many QT)

-- Quantization table
def QT =
  block
    let info = UInt8
    number   = truncate8to4 info
    let precision = info >> 4
    data =
      Choose1
        bit8  = { precision == 0 is true; Many 64 UInt8; }
        bit16 = { precision == 1 is true; Many 64 BE16;  }

def DRI =
  block
    Marker 0xDD
    Payload BE16

def SomeRST =
  block
    $$ = SomeMarker 0xD
    ($$ < 8) is true

def EntropyEncodedEntry =
  Choose1
    restart = SomeRST
    bytes   = Many (1..) EntropyEncodedByte

def EntropyEncodedByte =
  Choose1
    block $$ = UInt8 0xFF; UInt8 0x00
    UInt8 (!0xFF)



--------------------------------------------------------------------------------
-- JFIF Segment

def JFIF_APP0 = APP 0 {
  Match "JFIF\0";
  versionMajor = UInt8;
  versionminor = UInt8;
  densityUnits = DensityUnits;
  xDensity     = NonZero BE16;
  yDensity     = NonZero BE16;
  @xThumbnail  = UInt8 as uint 64;
  @yThumbnail  = UInt8 as uint 64;
  thumbnailData = Many (xThumbnail * yThumbnail) RGB;
}

def RGB = {
  r = UInt8;
  g = UInt8;
  b = UInt8;
}

def DensityUnits =
  Choose1
    NoUnits     = Match1 0
    Inches      = Match1 1
    Centimeters = Match1 2
  <| Fail "Invalid density unit"


--------------------------------------------------------------------------------
-- Exif Segment

def Exif_APP1 = APP 1
  block
    Match "Exif\0\0"
    let data_start = GetStream
    tiff_header = TIFFHeader
    ifd0 = IFD tiff_header.e
    other_data = Many UInt8

def TIFFHeader = block
  e = Endian
  Word16 e == 0x2A is true
  start = Word32 e
  start == 8 is true      -- assume first comes right after

def Endian = Choose1
  LE = @Match "II"
  BE = @Match "MM"

def IFD e =
  block
    let entryNum = Word16 e as uint 64
    entries      = Many entryNum (IFDEntry e)
    next         = Word32 e

def IFDEntry e = Choose1
  imageWidth  = NumericTag e 0x100
  imageHeight = NumericTag e 0x101
  -- bits per sample
  compression = NumericTag e 0x103    -- 1 = none, 6 = jpeg
  photometricInterpretation = NumericTag e 0x106

  samplesPerPixel = NumericTag e 0x115
  rowsPerStrip    = NumericTag e 0x116

  unknown     = IFDUnknown e

def Tag e x = Word16 e == x is true
def NumericTag e x = block
  Tag e x
  Unsgned e

def Unsgned e = block
  let format = Word16 e
  let items  = Word32 e
  items == 1 is true
  case format of
    1 -> block $$ = UInt8    as uint 64; Many 3 UInt8
    3 -> block $$ = Word16 e as uint 64; Many 2 UInt8
    4 -> Word32 e as uint 64

def IFDUnknown e = block
  tag    = Word16 e
  format = Word16 e as uint 64
  items  = Word32 e as uint 64
  data   = Word32 e


--------------------------------------------------------------------------------
-- Misc. utilities

def jn (e : Endian) x y =
  case e of
    BE -> x # y
    LE -> y # x

def Word16 e = jn e UInt8 UInt8
def Word32 e = jn e (Word16 e) (Word16 e)

def BE16 = UInt8 # UInt8

def NonZero P =
  block
    $$ = P;
    $$ > 0 is true;



















--------------------------------------------------------------------------------
-- Another way to check segments

def CheckSegments = block
  let start = UInt8
  (start == 0xFF is true) <|
        Fail (concat [ "Invalid byte ", showByte start, " expected 0xFF" ])
  let tag = UInt8
  case tag of
    0xFE -> block -- COM
      Payload (Many UInt8)
      CheckSegments

    0xC0, 0xC1, 0xC2, 0xC3, 0xC5, 0xC6, 0xC7,
      0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF -> block
        SOFPayload
        CheckSegments

    0xC4 -> block -- DHT
      Payload (Many HT)
      CheckSegments


    0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7 -> -- RST
      CheckSegments

    0xD9 -> {} -- EOI

    0xDA -> block -- SOS
      Payload SOSHeader
      Many EntropyEncodedEntry
      CheckSegments

    0xDB -> block
      Payload (Many QT)
      CheckSegments       -- DQT

    0xDD -> block -- DRI
      Payload BE16
      CheckSegments

    0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,  -- APP
      0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF -> block
        Payload (Many UInt8)
        CheckSegments


    _ -> Fail (concat [ "Unknown marker: ", showByte tag ])


def showByte (x : uint 8) =
  [ '0', 'x', showHexDigit (x >> 4), showHexDigit (x .&. 0x0F) ]

def showHexDigit (x : uint 8) = if x < 10 then '0' + x else 'a' + x - 10


