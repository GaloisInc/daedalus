import JpegBasics

def Main = {
  SOI;
  header = JFIF_APP0;
  segments = Many Segment;
  EOI;
}

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

def DensityUnits = Choose1 {
  NoUnits     = Match1 0;
  Inches      = Match1 1;
  Centimeters = Match1 2;
} <| Fail "Invalid density unit"

