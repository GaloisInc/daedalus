import nitf_lib

-- functions over pure values:

-- DOC: is the result of an leq or equality test a raw value or a grammar

-- DOC: discuss type annotations vs. coercions

-- DOC: list introduction forms

-- TODO: check uses of this
def Maybe guard P =
  { guard ; @v = P ; ^ just v} <|
  ^ nothing

def DateDefaultSpaces = DefaultSpaces 8 PartialDate

-- image subheader fields:

def Im = "IM"

def IID1 = Many 10 (AlphaNum | '_' | ' ')

def IDaTim = PartialDateTime

-- BE: basic encyclopedia
def BE = Many 10 AlphaNum

def OSuffix = Many 5 AlphaNum

def TgtId = {
  be = DefaultSpaces 10 BE ;
  osuffix = DefaultSpaces 5 OSuffix ;
  country = DefaultSpaces 2 CountryCode
}

def IID2 = Many 80 Byte

-- SeqWs: a sequence of P's of size k, padded by spaces to be n bytes
-- def SeqWs k n P = DefaultSpaces n { P ; SeqWs k (n - k) P }
-- DOC: why is this not a valid recursive type?

-- DOC: handling numeric literals

def ISorce = DefaultSpaces 42 (Many 42 Byte)

def NRows = PosNumber 8

def NCols = PosNumber 8

def PVType = Choose {
  integer = @(PadWSpaces 3 "INT") ;
  bilevel = @(PadWSpaces 3 "B") ;
  signed = @(PadWSpaces 3 "SI") ;
  real = @(PadWSpaces 3 "R") ;
  complex = @(PadWSpaces 3 "C") ;
}

def IRep = Choose {
  monochrome = @(PadWSpaces 8 "MONO") ;
  rgb = @(PadWSpaces 8 "RGB") ;
  rgblut = @(PadWSpaces 8 "RGB/LUT") ;
  multi = @(PadWSpaces 8 "MULTI") ; 
  nodisplay = @(PadWSpaces 8 "NODISPLY") ;
  cartesian = @(PadWSpaces 8 "NVECTOR") ;
  polar = @(PadWSpaces 8 "POLAR") ;
  sar = @(PadWSpaces 8 "VPH") ; 
  itur = @(PadWSpaces 8 "YCbCr601") ;
}

def ICat = Choose {
  visible = @(PadWSpaces 8 "VIS") ;
  sideLooking = @(PadWSpaces 8 "SL") ;
  thermalInfrared = @(PadWSpaces 8 "TI") ;
  forwardLooking = @(PadWSpaces 8 "FL") ;
  radar = @(PadWSpaces 8 "RD") ;
  electroOptical = @(PadWSpaces 8 "EO") ;
  optical = @(PadWSpaces 8 "OP") ;
  highResolution = @(PadWSpaces 8 "HR") ;
  hyperSpectral = @(PadWSpaces 8 "HS") ;
  colorPhoto = @(PadWSpaces 8 "CP") ;
  blackWhitePhoto = @(PadWSpaces 8 "BP") ;
  synthApertureRadar = @(PadWSpaces 8 "SAR") ;
  sarRadioHologram = @(PadWSpaces 8 "SARIQ") ;
  infrared = @(PadWSpaces 8 "IR") ;
  multiSpectral = @(PadWSpaces 8 "MS") ;
  fingerprints = @(PadWSpaces 8 "FP") ;
  mri = @(PadWSpaces 8 "MRI") ;
  xray = @(PadWSpaces 8 "XRAY") ;
  catScans = @(PadWSpaces 8 "CAT") ;
  video = @(PadWSpaces 8 "VD") ;
  barometric = @(PadWSpaces 8 "BARO") ;
  waterCurrent = @(PadWSpaces 8 "CURRENT") ;
  waterDepth = @(PadWSpaces 8 "DEPTH") ;
  airWind = @(PadWSpaces 8 "WIND") ;
  -- geographic products:
  rasterMap = @(PadWSpaces 8 "MAP") ;
  colorPatch = @(PadWSpaces 8 "PAT") ;
  legends = @(PadWSpaces 8 "LEG") ;
  elevationModel = @(PadWSpaces 8 "DTEM") ;
  otherMatrix = @(PadWSpaces 8 "MATR") ;
  locationGrid = @(PadWSpaces 8 "LOCG") ;
}

def ABPP = BoundedNum 2 1 96

def PJust = Choose {
  leftJust = @'L' ;
  rightJust = @'R' ;
}

def ICords = DefaultSpace (
  Choose {
    utm = @'U' ;
    northernhemi = @'N' ;
    southernhemi = @'S' ;
    geographic = @'G' ;
    decimal = @'D' ;
  })

def LatDeg = {
  sign = Sign ;
  @whole_digs = Many 2 Digit ;
  whole = ^ numBase 10 whole_digs ;
  '.' ;
  @frac_digs = Many 3 Digit ;
  frac = ^ numBase 10 frac_digs ;
  whole < 90 | { whole == 90 ; frac == 0 }
}

def LongDeg = {
  sign = Sign ;
  @whole_digs = Many 3 Digit ;
  whole = ^ numBase 10 whole_digs ;
  '.' ;
  @frac_digs = Many 3 Digit ;
  frac = ^ numBase 10 frac_digs ;
  whole < 180 | { whole == 180 ; frac == 0 }
}

def Latitude = {
  digs = PadWSpaces 6 (Many (..6) Numeral) ;
  hemi = Choose {
    north = @'N' ;
    south = @'S' ;
  }
}

def Longitude = {
  digs = PadWSpaces 7 (Many (..7) Numeral) ;
  hemi = Choose {
    east = @'E' ;
    west = @'W' ;
  }
}

def LatLong = {
  lat = Latitude ;
  long = Longitude
}

def UtmZone = {
  zone = BoundedNum 2 1 60 ;
}

def FiveDigitNum = {
  @v = Many 5 Digit ;
  ^ numBase 10 v
} 

def PlainUtm = {
  utm = Many 2 Numeral ;
  easting = Many 6 Numeral ;
  northing = Many 7 Numeral ;
}

def OmitIO lb ub =
  lb .. 'H' 
| 'J' .. 'N' 
| 'P' .. ub

def MGRS = {
  zone_num = Many 2 Digit ;
  zone_band = OmitIO 'C' 'X';
  sq_id = {
    col_id = OmitIO 'A' 'Z' ;
    row_id = OmitIO 'A' 'V' ;
  } ;
  easting = Many 5 Numeral ;
  northing = Many 5 Numeral
}

def EqLat l0 l1 = {
  l0.sign == l1.sign ;
  l0.whole == l1.whole ;
  l0.frac == l1.frac
}

-- TODO: refine this to allow only rectangles or triangles
def OrdLong left right =
  { left.sign == '-' ; right.sign == '+' }
| { left.sign == right.sign ;
    (  { left.sign == '-' ;
           left.whole > right.whole
         | { left.whole == right.whole ;
             left.frac >= right.frac } }
     | { left.sign == '+' ;
           left.whole < right.whole
         | { left.whole == right.whole ;
             left.frac <= right.frac } } ) }

def IGeoLo = Choose {
  decimal_degs = {
    lat0 = LatDeg ;
    long0 = LongDeg ;
    lat1 = LatDeg ;
    long1 = LongDeg ;
    lat2 = LatDeg ;
    long2 = LongDeg ;
    lat3 = LatDeg ;
    long3 = LongDeg ;
    EqLat lat0 lat1 ;
    EqLat lat2 lat3 ;
    OrdLong long0 long1 ;
    OrdLong long3 long2
  } ;
  lat_long = Many 4 LatLong ;
  mgrs = Many 4 MGRS ;
  plain_utm = Many 4 PlainUtm
}

def NICom = Digit

def IComn n = Many n (Many 80 Byte)

def IC = Choose {
  c1 = @"C1" ;
  c3 = @"C3" ;
  c4 = @"C4" ;
  c5 = @"C5" ;
  c6 = @"C6" ;
  c7 = @"C7" ;
  c8 = @"C8" ;
  i1 = @"I1" ;
  m1 = @"M1" ;
  m3 = @"M3" ;
  m4 = @"M4" ;
  m5 = @"M5" ;
  m6 = @"M6" ;
  m7 = @"M7" ;
  m8 = @"M8" ;
  nc = @"NC" ;
  nm = @"NM" ;
}

def ComRat ic = Choose {
  dim_coding = { 
      ic is c1
    | ic is m1 ;
    Choose {
      oned = @(PadWSpaces 4 "1D") ;
      twods = @(PadWSpaces 4 "2DS") ;
      twodh = @(PadWSpaces 4 "2DH") ;
    } ;
  } ; 
  quant_tables = {
      ic is c3
    | ic is c5
    | ic is i1
    | ic is m3
    | ic is m5 ;
    '0' ;
    img_data_type = BoundedDigit 0 4;
    '.' ;
    quality_level = BoundedDigit 0 5;
      {   ic is c5
        | ic is m5 ;
        quality_level == 0 }
    | {   ic is c3
        | ic is i1
        | ic is m3 }
  } ; 
  bits_per_pixel = {
      ic is c4
    | ic is m4 ;
    ones = Digit ;
    '.' ;
    tenths = Digit ;
    hudredths = Digit
  } ;
  nominal = {
      ic is c8
    | ic is m8 ;
    Many 4 Byte
  }
}


def NBands irep = (
  { irep is nodisplay ;
    Digit ;
  }
| { irep is monochrome ;
    IsNum 1 1
  }
| { irep is rgb ;
    IsNum 1 3
  }
| { irep is rgblut ;
    IsNum 1 1
  }
| { irep is itur ;
    IsNum 3 3
  }
| { irep is cartesian ;
    Digit
  }
| { irep is polar ;
    IsNum 1 2
  }
| { irep is sar ;
    IsNum 1 1
  }
| { irep is multi ;
      IsNum 1 0
    | BoundedNum 1 2 9
  }
| IsNum 1 0
)
  
def XBands n = BoundedNum 5 10 99999

def IRepBandN = Choose {
  bandM = @(PadWSpaces 2 "M") ;
  lutBand = @(PadWSpaces 2 "LU") ;
  red = @(PadWSpaces 2 "R") ;
  green = @(PadWSpaces 2 "G") ;
  blue = @(PadWSpaces 2 "B") ;
  monoBand = @(PadWSpaces 2 "M") ;
  luminance = @(PadWSpaces 2 "Y") ;
  chrominanceBlue = @(PadWSpaces 2 "Cb") ;
  chrominanceRed = @(PadWSpaces 2 "Cr") ;
  default = @(Spaces 2) ;
}

def ISubCatN = Choose {
  inphase = @(PadWSpaces 6 "I") ;
  quadrature = @(PadWSpaces 6 "Q") ;
  magnitude = @(PadWSpaces 6 "M") ;
  phase = @(PadWSpaces 6 "P") ;
  speed = @(PadWSpaces 6 "SPEED") ;
  direct = @(PadWSpaces 6 "DIRECT") ;
  easting = @(PadWSpaces 6 "CGX") ;
  northing = @(PadWSpaces 6 "CGY") ;
  longitude = @(PadWSpaces 6 "GGX") ;
  latitude = @(PadWSpaces 6 "GGY") ;
  waveLength = UnsignedNum 6 ;
  default = @(Spaces 6) ;
  userdef = Many 6 BCSA ;
}
  
def IFCN = 'N'  -- other values reserved for future use

def ImFltN = Spaces 3 -- reserved for future use

def NELutN = BoundedPos 5 65536

def LutdNM n = Many n Byte

def ISync = '0' -- reserved for future use

def IMode nbands = {
  $$ = Choose {
    block = @'B' ;
    pixel = @'P' ;
    row = @'R' ;
    seq = @'S' ;
  } ;
  nbands != 1 | $$ is block
}

def NBPR = PosQuad

def NBPC = PosQuad

def NPPBH = UpperBounded 4 8192

def NPPBV = UpperBounded 4 8192

def NBPP abpp ic = {
  $$ = BoundedPos 2 96 ;
  $$ >= abpp ;
    {   ic is c3
      | ic is c5
      | ic is i1
      | ic is m3
      | ic is m5 ;
        $$ == 8 
      | $$ == 12 }
  | { ic is c1 ;
      $$ == 1 }
  | {   ic is c1
      | ic is m8 ;
      1 <= $$ ; $$ <= 38 }
  | {   ic is c4
      | ic is c6
      | ic is c7
      | ic is c8
      | ic is m1
      | ic is m4
      | ic is m6
      | ic is m7
      | ic is nc
      | ic is nm }
}

def IDLvl = BoundedNum 3 1 999

-- TODO: this and image display level need to satisfy a global
-- property over all image segments

def IALvl = AttachmentLvl

def IMag = Choose {
  fp = {
    $$ = FixedPoint ;
    @fplen = ^ (strlen $$.digs) + 1 + (strlen $$.radix) ;
    -- DOC: why was this hard to refactor?
    fplen <= 4 ;
    Spaces (4 - fplen)
  } ;
  frac = { 
    '/' ;
    $$ = Many (..3) Digit ;
    @fplen = ^ (strlen $$) + 1 ;
    fplen <= 4 ;
    Spaces (4 - fplen) 
  }
}

def UDIDL = LowerBoundedOrZero 5 3 

def UDOfl = UnsignedNum 3

def UDID n = Many (n - 3) Byte

def IXShDL = LowerBoundedOrZero 5 3 

def IXSOfl = UnsignedNum 3

def IXShD n = Many (n - 3) Byte

-- encoding of display-dependent parameters (Table A-2)
def DispParams (irep : IRep) (irepband : IRepBandN) nbands pvtype nluts =
  { irep is nodisplay ;
    irepband is default ;
      { 1 <= nbands ; nbands <= 9 }
    | nbands == 0 ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex
    | pvtype is bilevel
    | pvtype is signed ;
    nluts == 0 }
| { irep is monochrome ;
      irepband is lutBand
    | irepband is monoBand
    | irepband is default;
    nbands == 1;
      pvtype is integer
    | pvtype is real
    | pvtype is bilevel ;
    0 <= nluts ; nluts <= 2 }
| { irep is rgb ;
      irepband is red
    | irepband is green
    | irepband is blue ;
    nbands == 3;
      pvtype is integer
    | pvtype is real ;
    nluts == 0 }
| { irep is rgblut ;
    irepband is lutBand ;
    nbands == 1 ;
      pvtype is integer
    | pvtype is real ;
    nluts == 3 }
| { irep is itur ;
      irepband is luminance
    | irepband is chrominanceBlue
    | irepband is chrominanceRed ;
    nbands == 3 ;
    pvtype is integer ;
    nluts == 0 }
| { irep is cartesian ;
    irepband is default ;
      { 1 <= nbands ; nbands <=  9 }
    | nbands == 0 ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex ;
    nluts == 0 }
| { irep is polar ;
      irepband is default
    | irepband is monoBand ;
    nbands == 2 ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex ;
    nluts == 0 }
| { irep is sar ;
    irepband is default ;
    nbands == 2 ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex ;
    nluts == 0 }
| { irep is multi ;
      irepband is default
    | irepband is monoBand
    | irepband is red
    | irepband is green
    | irepband is blue
    | irepband is lutBand ;
      { 2 <= nbands ; nbands <= 9 }
    | nbands == 0 ;
    0 <= nluts ; nluts <= 3
  }

def CatIntLow nbpp abpp = {
  nbpp == 8 ;
  2 <= abpp ; nbpp <= 8
}

def CatIntMid nbpp abpp =
  { nbpp == 12 ;
    8 <= abpp ; nbpp <= 12 }
| { nbpp == 16 ;
    9 <= abpp ; nbpp <= 16 }

def CatIntHigh nbpp abpp =
  { nbpp == 32 ;
    17 <= abpp ; nbpp <= 32 }
| { nbpp == 64 ;
    33 <= abpp ; nbpp <= 64 }

def CatIntEnds nbpp abpp =
  CatIntLow nbpp abpp
| CatIntHigh nbpp abpp

def CatIntFull nbpp abpp =
  CatIntEnds nbpp abpp
| CatIntMid nbpp abpp

def CatReals nbpp abpp =
  { nbpp == 32 ; abpp == 32 }
| { nbpp == 64 ; abpp == 64 }

def CatComplex nbpp abpp = {
  nbpp == 64 ;
  abpp == 64
}

-- CatParams: formalization of Table A-2(A)
-- DOC: is there a way to do total pattern matching on a sum type?
def CatParams (icat : ICat) (isubcat : ISubCatN) nbands (pvtype : PVType) nbpp abpp =
  {   icat is visible
    | icat is optical ;
      isubcat is default
    | @(isubcat is userdef) ;
      { nbands == 1 ;
        pvtype is bilevel ;
        nbpp == 1 ;
        abpp == 1 }
    | {   nbands == 1
        | nbands == 3 ;
          { pvtype is integer ; CatIntFull nbpp abpp } 
        | { pvtype is real ; CatReals nbpp abpp } } }
| {   icat is sideLooking
    | icat is thermalInfrared
    | icat is forwardLooking
    | icat is radar
    | icat is electroOptical
    | icat is highResolution
    | icat is blackWhitePhoto
    | icat is fingerprints
    | icat is video
    | icat is catScans
    | icat is mri
    | icat is xray ;
      isubcat is default
    | @(isubcat is userdef) ;
    nbands == 1 ;
      { pvtype is integer ; CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| { icat is infrared ;
    nbands == 1 ;
      { pvtype is integer ; CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| {   icat is colorPhoto 
    | icat is colorPatch ;
      isubcat is default
    | @(isubcat is userdef) ;
    nbands == 3 ;
    pvtype is integer ; CatIntEnds nbpp abpp }
| {   icat is rasterMap
    | icat is legends ;
      isubcat is default
    | @(isubcat is userdef) ;
      nbands == 1
    | nbands == 3 ;
    pvtype is integer ; CatIntEnds nbpp abpp }
| { icat is locationGrid ;
      isubcat is easting
    | isubcat is northing
    | isubcat is longitude
    | isubcat is latitude ;
    nbands == 2 ;
      {   pvtype is integer
        | pvtype is signed ;
        CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| { icat is otherMatrix ;
    isubcat is userdef ;
    1 <= nbands ; nbands <= 9 ;
      { pvtype is complex ; CatComplex nbpp abpp }
    | {   pvtype is integer
        | pvtype is signed ;
        CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| {   icat is multiSpectral
    | icat is hyperSpectral ;
      @(isubcat is waveLength)
    | isubcat is default ; -- required for milsamples, but does it match spec?
      { 2 <= nbands ; nbands <= 9 }
    | nbands == 0 ;
      {   pvtype is integer
        | pvtype is signed ;
        CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| {   icat is synthApertureRadar
    | icat is sarRadioHologram ;
      isubcat is inphase
    | isubcat is quadrature
    | isubcat is magnitude
    | isubcat is phase
    | isubcat is default ;
      { nbands == 1 ; pvtype is complex ; CatComplex nbpp abpp }
    | {   nbands == 1
        | nbands == 2 ;
          {   pvtype is integer
            | pvtype is signed ;
            CatIntFull nbpp abpp }
        | { pvtype is real ; CatReals nbpp abpp } } }
| {   icat is airWind
    | icat is waterCurrent ;
      isubcat is speed
    | isubcat is direct ;
    nbands == 2 ;
    pvtype is integer ;
    nbpp == 8 ;
    2 <= abpp ; abpp <= 8 }
| {   icat is barometric
    | icat is waterDepth ;
    nbands == 1 ;
    pvtype is integer ;
      CatIntLow nbpp abpp
    | CatIntMid nbpp abpp }
| { icat is elevationModel ;
    nbands == 1 ;
      {   pvtype is integer
        | pvtype is signed ;
        CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }

-- ISHeader: an image segment header
def ISHeader = {
  Im ;
  iid1 = IID1 ;
  idatim = IDaTim ;
  tgt_id = TgtId ;
  iid2 = IID2 ;

  -- TODO: these fields overlap with many other subheaders and could
  -- be refactored
  common = CommonSubheader ;

  Encryp ;

  i_sorce = ISorce ;
  n_rows = NRows ;
  n_cols = NCols ;
  pvtype = PVType ;
  irep = IRep ;
  icat = ICat ;
  abpp = ABPP ;
  p_just = PJust ;

  icords = ICords ;
  Choose {
    igeolo = {
      icords is actual ;
      IGeoLo
    } ;
    empty = icords is default
  };

  nicom = NICom ;
  i_com_n = IComn nicom ;

  ic = IC ;

  Choose {
    comrat = {
        ic is c1
      | ic is c3
      | ic is c4
      | ic is c5
      | ic is c6
      | ic is c8
      | ic is m1
      | ic is m3
      | ic is m4
      | ic is m5
      | ic is m8
      | ic is i1 ;
      ComRat ic
    } ;
    empty =
        ic is nc
      | ic is nm
  } ;

  nbands = NBands irep ;

  xbands = Choose {
    def_xband = {
      nbands == 0 ;
      XBands nbands
    } ;
    no_xband = nbands != 0 
  } ;

  @num_bands =
    { nbands != 0 ;
      ^ nbands }
  | { nbands == 0 ;
      @bnds = xbands is def_xband ;
      ^ bnds
    } ;

  bandinfo = Many num_bands {
    irepbandn = IRepBandN ;
    isubcatn = ISubCatN ;

    IFCN ;
    ImFltN ;

    nlutsn = UpperBoundedDigit 4 ;

    -- DOC: error message here could say more: thought that problem
    -- was in how parser was used. Problem was actually buried in some
    -- field of spaces.

    -- check display params
    DispParams irep irepbandn nbands pvtype nlutsn ;

    Choose {
      luts = {
        nlutsn > 0 ;
        nelutn = NELutN ;
        lutd_nm = LutdNM nlutsn ;
      } ;
      no_luts = nlutsn == 0 
    } 
  } ;

  ISync ;

  imode = IMode nbands ;
  nbpr = NBPR ;
  -- TODO: rework to remove negations
  nbpc = NBPC ;
    (nbpr != 1 | nbpc != 1)
  | (imode is block | imode is pixel | imode is row);
  -- is R really allowed? Needed by i_3201c.ntf.

  nppbh = NPPBH ;
  nbpr * nppbh >= n_cols ;

  nppbv = NPPBV ;
  nbpc * nppbv >= n_rows ;

  nbpp = NBPP abpp ic ;

  -- check all category-dependent parameters:
  map
    (bi in bandinfo)
    (CatParams icat bi.isubcatn nbands pvtype nbpp abpp) ;

  idlvl = IDLvl ;
  ialvl = IALvl ;
  iloc = Location ;
  imag = IMag ;

  udidl = UDIDL ;
  Choose {
    uds = {
      udidl > 0 ;
      udofl = UDOfl ;
      udid = UDID udidl
    } ;
    empty = udidl == 0
  } ;

  ixshdl = IXShDL ;
  Choose {
    ixs = {
      ixshdl > 0 ;
      ixsofl = IXSOfl ;
      ixshd = IXShD ixshdl
    } ;
    empty = ixshdl == 0
  }
}
