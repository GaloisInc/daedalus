def Marker x = {
  Match1 0xFF;
  @Match1 x;
}

-- For families of markers, such as APP or SOF
def SomeMarker (front : uint 4) = {
  Match1 0xFF;
  @tag = UInt8;
  @upper = (tag >> 4) as! uint 4;
  (upper == front) is true;
  tag as! uint 4;
}

def BE16 = UInt8 # UInt8

def Only P = { $$ = P; END }

def NonZero P = {
  $$ = P;
  $$ > 0 is true;
}

def Payload P = {
  @size = BE16 as uint 64;
  size >= 2 is true;
  @len = size - 2;
  @here = GetStream;
  SetStream (Take len here);
  $$ = P;
  SetStream (Drop len here);
}


-- Start / End image
def SOI = Marker 0xD8
def EOI = Marker 0xD9

-- Comment
def COM = { Marker 0xFE; Payload GetStream }

-- Application specific
def APP (x : uint 4) P = {
  Marker (0xE # x);
  Payload P;
}

-- Application specific, uninterpreted
def SomeAPP = {
  app  = SomeMarker 0xE;
  data = Payload GetStream;
}

-- Start of frame (x /= 4)
def SOF (x : uint 4) = {
  Marker (0xC # x);
  SOFPayload;
}

-- Start of frame, uninterpreted
def SomeSOF = {
  sof  = SomeMarker 0xC;
  data = SOFPayload;
}


def SOFPayload = Payload {
  samplePrecision        = UInt8;
  numberOfLines          = BE16;
  numberOfSamplesPerLine = BE16;
  @comNumber             = UInt8 as uint 64;
  components             = Many comNumber FrameComponent;
}


def FrameComponent = {
  identifier        = UInt8;
  @samplingFactors  = UInt8;
  hoizontalSampling = (samplingFactors >> 4) as! uint 4;
  verticalSampling  = samplingFactors        as! uint 4;
  quantTableSel     = UInt8;
}


-- Start of Scan
def SOS = {
  Marker 0xDA;
  Payload GetStream;
  SkipEntropyEncodedData;
}


-- Define Huffman tbale
def DHT = {
  Marker 0xC4;
  Payload GetStream; -- XXX
}

-- Define quantization table
def DQT = {
  Marker 0xDB;
  Payload GetStream; -- XXX
}


def DRI = {
  Marker 0xDD;
  Payload (Only BE16);
}


-- Hack
def SkipEntropyEncodedData = {
  @here = GetStream;
  @byte = UInt8;
  case byte is {
    0xFF -> {
        @byte1 = UInt8;
        case byte1 is {
          0x00 -> SkipEntropyEncodedData; -- Escape
          0xDD -> SkipEntropyEncodedData; -- Restart
          _    -> SetStream here;
        }
      };
    _ -> SkipEntropyEncodedData
  }
}


def Segment = Choose1 {
  comment = COM;
  dri     = DRI;
  sof     = SomeSOF;
  sos     = SOS;
  app     = SomeAPP;
  dqt     = DQT;
  dht     = DHT;
}

