-- Reference: https://www.w3.org/Graphics/JPEG/itu-t81.pdf

def Marker x = Match [ 0xFF, x ]

-- For families of markers, such as APP or SOF
def SomeMarker (front : uint 4) =
  block
    UInt8 0xFF
    let tag   = UInt8
    let upper = (tag >> 4) as! uint 4
    upper == front is true
    tag as! uint 4

def BE16 = UInt8 # UInt8

def NonZero P =
  block
    $$ = P;
    $$ > 0 is true;

def Payload P =
  block
    let size = BE16 as uint 64
    size >= 2 is true
    let len = size - 2
    let here = GetStream
    SetStream (Take len here)
    $$ = P
    END
    SetStream (Drop len here)


-- Start / End image
def SOI = Marker 0xD8 <| Fail "Missing Start-of-Image"
def EOI = Marker 0xD9 <| Fail "Missing End-of-Image"

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
def SomeAPP =
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
    hoizontalSampling   = (samplingFactors >> 4) as! uint 4
    verticalSampling    = samplingFactors        as! uint 4
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
    ah = a >> 4 as! uint 4
    al = a      as! uint 4

def SOSComponent =
  block
    id      = UInt8
    let table  = UInt8
    acTable = table        as! uint 4
    dcTable = (table >> 4) as! uint 4


-- Define Huffman tables
def DHT =
  block
    Marker 0xC4
    commit
    Payload (Many HT)

-- Huffman table
def HT =
  block
    let info    = UInt8
    class       = info as! uint 4
    type        = (info >> 4) as! uint 4    -- 0 = DC, 1 = AC
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
    number   = info as! uint 4
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
    segment = Segment
    bytes   = Many (1..) EntropyEncodedByte

def EntropyEncodedByte =
  Choose1
    block $$ = UInt8 0xFF; UInt8 0x00
    UInt8 (!0xFF)



def Segment =
  Choose1
    comment = COM;
    dri     = DRI;
    sof     = SomeSOF;
    sos     = SOS;
    app     = SomeAPP;
    dqt     = DQT;
    dht     = DHT;
    rst     = SomeRST;

def SomeJpeg = block SOI; $$ = Many Segment; EOI
