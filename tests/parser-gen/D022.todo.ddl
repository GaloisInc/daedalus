-- test fragment from Jpeg

def Marker x = Match [ 0xFF, x ]

-- For families of markers, such as APP or SOF
def SomeMarker (front : uint 4) =
  block
    $[0xFF]
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
    let there = Take len here
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

def DRI =
  block
    Marker 0xDD
    Payload BE16


-- Start of Scan SOS
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

def SomeRST =
  block
    $$ = SomeMarker 0xD
    ($$ < 8) is true

def EntropyEncodedEntry =
  --First
  --  restart = SomeRST
  --  bytes   = Many (1..) EntropyEncodedByte
    Many (1..) EntropyEncodedByte

def EntropyEncodedByte =
  First
    block $$ = $[0xFF]; $[0x00]
    $[!0xFF]


def Segment =
  First
    comment = COM
    dri     = DRI
    -- sof     = SomeSOF
    -- sos     = SOS
    -- app     = SomeAPP
    -- dqt     = DQT
    -- dht     = DHT
    -- rst     = SomeRST

def SomeJpeg = block SOI; $$ = Many Segment; EOI

def Main = block SomeJpeg; END
