-- Reference: https://www.w3.org/Graphics/JPEG/itu-t81.pdf

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
  END;
  SetStream (Drop len here);
}


-- Start / End image
def SOI = Marker 0xD8
def EOI = Marker 0xD9

-- Comment
def COM = { Marker 0xFE; Payload (Many UInt8); }

-- Application specific
def APP (x : uint 4) P = {
  Marker (0xE # x);
  Payload P;
}

-- Application specific, uninterpreted
def SomeAPP = {
  app  = SomeMarker 0xE;
  data = Payload (Many UInt8);
}

-- Start of frame (x /= 4)
def SOF (x : uint 4) = {
  Marker (0xC # x);
  SOFPayload;
}

-- Start of frame, uninterpreted
def SomeSOF = {
  sof  = SomeMarker 0xC;
  sof == 4 is false;
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
  $$ = Payload SOSHeader;
  SkipEntropyEncodedData;
}

def SOSHeader = {
  @componentNum = UInt8;
  components = Many (componentNum as uint 64) SOSComponent;
  ss = UInt8;
  se = UInt8;
  @a = UInt8;
  ah = a >> 4 as! uint 4;
  al = a      as! uint 4;
}

def SOSComponent = {
  id = UInt8;
  @table = UInt8;
  acTable = table        as! uint 4;
  dcTable = (table >> 4) as! uint 4;
}


-- Define Huffman tables
def DHT = {
  Marker 0xC4;
  Payload (Many HT);
}

-- Huffman table
def HT = {
  @info = UInt8;
  class = info as! uint 4;
  type  = (info >> 4) as! uint 4;    -- 0 = DC, 1 = AC
  @symNums = Many 16 UInt8;
  table = map (n in symNums) (Many (n as uint 64) UInt8)
} <| Fail "Malformed Huffman table"

-- Define quantization tables
def DQT = {
  Marker 0xDB;
  Payload (Many QT);
}

-- Quantization table
def QT = {
  @info = UInt8;
  number = info as! uint 4;
  @precision = info >> 4;
  data = Choose1 {
           bit8  = { precision == 0 is true; Many 64 UInt8; };
           bit16 = { precision == 1 is true; Many 64 BE16;  };
         }
}


def DRI = {
  Marker 0xDD;
  Payload BE16;
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

