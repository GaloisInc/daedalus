import nitf_lib

-- functions over pure values:

-- DOC: is the result of an leq or equality test a raw value or a grammar

-- DOC: discuss type annotations vs. coercions

-- DOC: list introduction forms

-- TODO: check uses of this
def Maybe guard P =
  { Match guard ; @v = P ; ^ just v} <|
  ^ nothing

def DateDefaultSpaces = DefaultSpaces 8 PartialDate

-- image subheader fields:

def Im = Match "IM"

def IID1 = Many 10 (AlphaNum | Match1 ('_' | ' '))

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
  integer = @(PadWSpaces 3 (Match "INT")) ;
  bilevel = @(PadWSpaces 3 (Match "B")) ;
  signed = @(PadWSpaces 3 (Match "SI")) ;
  real = @(PadWSpaces 3 (Match "R")) ;
  complex = @(PadWSpaces 3 (Match "C")) ;
}

def IRep = Choose {
  monochrome = @(PadWSpaces 8 (Match "MONO")) ;
  rgb = @(PadWSpaces 8 (Match "RGB")) ;
  rgblut = @(PadWSpaces 8 (Match "RGB/LUT")) ;
  multi = @(PadWSpaces 8 (Match "MULTI")) ; 
  nodisplay = @(PadWSpaces 8 (Match "NODISPLY")) ;
  cartesian = @(PadWSpaces 8 (Match "NVECTOR")) ;
  polar = @(PadWSpaces 8 (Match "POLAR")) ;
  sar = @(PadWSpaces 8 (Match "VPH")) ; 
  itur = @(PadWSpaces 8 (Match "YCbCr601")) ;
}

def ICat = Choose {
  visible = @(PadWSpaces 8 (Match "VIS")) ;
  sideLooking = @(PadWSpaces 8 (Match "SL")) ;
  thermalInfrared = @(PadWSpaces 8 (Match "TI")) ;
  forwardLooking = @(PadWSpaces 8 (Match "FL")) ;
  radar = @(PadWSpaces 8 (Match "RD")) ;
  electroOptical = @(PadWSpaces 8 (Match "EO")) ;
  optical = @(PadWSpaces 8 (Match "OP")) ;
  highResolution = @(PadWSpaces 8 (Match "HR")) ;
  hyperSpectral = @(PadWSpaces 8 (Match "HS")) ;
  colorPhoto = @(PadWSpaces 8 (Match "CP")) ;
  blackWhitePhoto = @(PadWSpaces 8 (Match "BP")) ;
  synthApertureRadar = @(PadWSpaces 8 (Match "SAR")) ;
  sarRadioHologram = @(PadWSpaces 8 (Match "SARIQ")) ;
  infrared = @(PadWSpaces 8 (Match "IR")) ;
  multiSpectral = @(PadWSpaces 8 (Match "MS")) ;
  fingerprints = @(PadWSpaces 8 (Match "FP")) ;
  mri = @(PadWSpaces 8 (Match "MRI")) ;
  xray = @(PadWSpaces 8 (Match "XRAY")) ;
  catScans = @(PadWSpaces 8 (Match "CAT")) ;
  video = @(PadWSpaces 8 (Match "VD")) ;
  barometric = @(PadWSpaces 8 (Match "BARO")) ;
  waterCurrent = @(PadWSpaces 8 (Match "CURRENT")) ;
  waterDepth = @(PadWSpaces 8 (Match "DEPTH")) ;
  airWind = @(PadWSpaces 8 (Match "WIND")) ;
  -- geographic products:
  rasterMap = @(PadWSpaces 8 (Match "MAP")) ;
  colorPatch = @(PadWSpaces 8 (Match "PAT")) ;
  legends = @(PadWSpaces 8 (Match "LEG")) ;
  elevationModel = @(PadWSpaces 8 (Match "DTEM")) ;
  otherMatrix = @(PadWSpaces 8 (Match "MATR")) ;
  locationGrid = @(PadWSpaces 8 (Match "LOCG")) ;
}

def ABPP = BoundedNum 2 1 96

def PJust = Choose {
  leftJust = @Match1 'L' ;
  rightJust = @Match1 'R' ;
}

def ICords = DefaultSpace (
  Choose {
    utm = @Match1 'U' ;
    northernhemi = @Match1 'N' ;
    southernhemi = @Match1 'S' ;
    geographic = @Match1 'G' ;
    decimal = @Match1 'D' ;
  })

def LatDeg = {
  sign = Sign ;
  @whole_digs = Many 2 Digit ;
  whole = ^ numBase 10 whole_digs ;
  Match1 '.' ;
  @frac_digs = Many 3 Digit ;
  frac = ^ numBase 10 frac_digs ;
  Guard (whole < 90) | { Guard (whole == 90); Guard (frac == 0) }
}

def LongDeg = {
  sign = Sign ;
  @whole_digs = Many 3 Digit ;
  whole = ^ numBase 10 whole_digs ;
  Match1 '.' ;
  @frac_digs = Many 3 Digit ;
  frac = ^ numBase 10 frac_digs ;
  Guard (whole < 180) | { Guard (whole == 180); Guard (frac == 0) }
}

def Latitude = {
  digs = PadWSpaces 6 (Many (..6) Numeral) ;
  hemi = Choose {
    north = @Match1 'N' ;
    south = @Match1 'S' ;
  }
}

def Longitude = {
  digs = PadWSpaces 7 (Many (..7) Numeral) ;
  hemi = Choose {
    east = @Match1 'E' ;
    west = @Match1 'W' ;
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

def OmitIO lb ub = Match1 (
  lb .. 'H' 
| 'J' .. 'N' 
| 'P' .. ub
)

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
  Guard (l0.sign == l1.sign) ;
  Guard (l0.whole == l1.whole) ;
  Guard (l0.frac == l1.frac)
}

-- TODO: refine this to allow only rectangles or triangles
--XXX: This can be one single expression
def OrdLong left right =
  { Guard (left.sign == '-') ; Guard (right.sign == '+') }
| { Guard (left.sign == right.sign) ;
    (  { Guard (left.sign == '-') ;
           Guard (left.whole > right.whole)
         | { Guard (left.whole == right.whole) ;
             Guard (left.frac >= right.frac) } }
     | { Guard (left.sign == '+') ;
           Guard (left.whole < right.whole)
         | { Guard (left.whole == right.whole) ;
             Guard (left.frac <= right.frac) } } ) }

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
  c1 = @Match "C1" ;
  c3 = @Match "C3" ;
  c4 = @Match "C4" ;
  c5 = @Match "C5" ;
  c6 = @Match "C6" ;
  c7 = @Match "C7" ;
  c8 = @Match "C8" ;
  i1 = @Match "I1" ;
  m1 = @Match "M1" ;
  m3 = @Match "M3" ;
  m4 = @Match "M4" ;
  m5 = @Match "M5" ;
  m6 = @Match "M6" ;
  m7 = @Match "M7" ;
  m8 = @Match "M8" ;
  nc = @Match "NC" ;
  nm = @Match "NM" ;
}

def ComRat ic = Choose {
  dim_coding = { 
      ic is c1
    | ic is m1 ;
    Choose {
      oned = @(PadWSpaces 4 (Match "1D")) ;
      twods = @(PadWSpaces 4 (Match "2DS")) ;
      twodh = @(PadWSpaces 4 (Match "2DH")) ;
    } ;
  } ; 
  quant_tables = {
      ic is c3
    | ic is c5
    | ic is i1
    | ic is m3
    | ic is m5 ;
    Match1 '0' ;
    img_data_type = BoundedDigit 0 4;
    Match1 '.' ;
    quality_level = BoundedDigit 0 5;
      {   ic is c5
        | ic is m5 ;
        Guard (quality_level == 0) }
    | {   ic is c3
        | ic is i1
        | ic is m3 }
  } ; 
  bits_per_pixel = {
      ic is c4
    | ic is m4 ;
    ones = Digit ;
    Match1 '.' ;
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
  bandM = @(PadWSpaces 2 (Match "M")) ;
  lutBand = @(PadWSpaces 2 (Match "LU")) ;
  red = @(PadWSpaces 2 (Match "R")) ;
  green = @(PadWSpaces 2 (Match "G")) ;
  blue = @(PadWSpaces 2 (Match "B")) ;
  monoBand = @(PadWSpaces 2 (Match "M")) ;
  luminance = @(PadWSpaces 2 (Match "Y")) ;
  chrominanceBlue = @(PadWSpaces 2 (Match "Cb")) ;
  chrominanceRed = @(PadWSpaces 2 (Match "Cr")) ;
  default = @(Spaces 2) ;
}

def ISubCatN = Choose {
  inphase = @(PadWSpaces 6 (Match "I")) ;
  quadrature = @(PadWSpaces 6 (Match "Q")) ;
  magnitude = @(PadWSpaces 6 (Match "M")) ;
  phase = @(PadWSpaces 6 (Match "P")) ;
  speed = @(PadWSpaces 6 (Match "SPEED")) ;
  direct = @(PadWSpaces 6 (Match "DIRECT")) ;
  easting = @(PadWSpaces 6 (Match "CGX")) ;
  northing = @(PadWSpaces 6 (Match "CGY")) ;
  longitude = @(PadWSpaces 6 (Match "GGX")) ;
  latitude = @(PadWSpaces 6 (Match "GGY")) ;
  waveLength = UnsignedNum 6 ;
  default = @(Spaces 6) ;
  userdef = Many 6 BCSA ;
}
  
def IFCN = Match1 'N'  -- other values reserved for future use

def ImFltN = Spaces 3 -- reserved for future use

def NELutN = BoundedPos 5 65536

def LutdNM n = Many n Byte

def ISync = Match1 '0' -- reserved for future use

def IMode nbands = {
  $$ = Choose {
    block = @Match1 'B' ;
    pixel = @Match1 'P' ;
    row = @Match1 'R' ;
    seq = @Match1 'S' ;
  } ;
  Guard (nbands != 1) | $$ is block
}

def NBPR = PosQuad

def NBPC = PosQuad

def NPPBH = UpperBounded 4 8192

def NPPBV = UpperBounded 4 8192

def NBPP abpp ic = {
  $$ = BoundedPos 2 96 ;
  Guard ($$ >= abpp) ;
    {   ic is c3
      | ic is c5
      | ic is i1
      | ic is m3
      | ic is m5 ;
        Guard ($$ == 8)
      | Guard ($$ == 12) }
  | { ic is c1 ;
      Guard ($$ == 1) }
  | {   ic is c1
      | ic is m8 ;
      Guard (1 <= $$) ; Guard ($$ <= 38) }
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
    Guard (fplen <= 4) ;
    Spaces (4 - fplen)
  } ;
  frac = { 
    Match1 '/' ;
    $$ = Many (..3) Digit ;
    @fplen = ^ (strlen $$) + 1 ;
    Guard (fplen <= 4) ;
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
      { Guard (1 <= nbands) ; Guard (nbands <= 9) }
    | Guard (nbands == 0) ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex
    | pvtype is bilevel
    | pvtype is signed ;
    Guard (nluts == 0) }
| { irep is monochrome ;
      irepband is lutBand
    | irepband is monoBand
    | irepband is default;
    Guard (nbands == 1);
      pvtype is integer
    | pvtype is real
    | pvtype is bilevel ;
    Guard (0 <= nluts) ; Guard (nluts <= 2) }
| { irep is rgb ;
      irepband is red
    | irepband is green
    | irepband is blue ;
    Guard (nbands == 3);
      pvtype is integer
    | pvtype is real ;
    Guard (nluts == 0) }
| { irep is rgblut ;
    irepband is lutBand ;
    Guard (nbands == 1) ;
      pvtype is integer
    | pvtype is real ;
    Guard (nluts == 3) }
| { irep is itur ;
      irepband is luminance
    | irepband is chrominanceBlue
    | irepband is chrominanceRed ;
    Guard (nbands == 3) ;
    pvtype is integer ;
    Guard (nluts == 0) }
| { irep is cartesian ;
    irepband is default ;
      { Guard (1 <= nbands) ; Guard (nbands <=  9) }
    | Guard (nbands == 0) ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex ;
    Guard (nluts == 0) }
| { irep is polar ;
      irepband is default
    | irepband is monoBand ;
    Guard (nbands == 2) ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex ;
    Guard (nluts == 0) }
| { irep is sar ;
    irepband is default ;
    Guard (nbands == 2) ;
      pvtype is integer
    | pvtype is real
    | pvtype is complex ;
    Guard (nluts == 0) }
| { irep is multi ;
      irepband is default
    | irepband is monoBand
    | irepband is red
    | irepband is green
    | irepband is blue
    | irepband is lutBand ;
      { Guard (2 <= nbands) ; Guard (nbands <= 9) }
    | Guard (nbands == 0) ;
    Guard (0 <= nluts) ; Guard (nluts <= 3)
  }

def CatIntLow nbpp abpp = {
  Guard (nbpp == 8) ;
  Guard (2 <= abpp) ; Guard (nbpp <= 8)
}

def CatIntMid nbpp abpp =
  { Guard (nbpp == 12) ;
    Guard (8 <= abpp) ; Guard (nbpp <= 12) }
| { Guard (nbpp == 16) ;
    Guard (9 <= abpp) ; Guard (nbpp <= 16) }

def CatIntHigh nbpp abpp =
  { Guard (nbpp == 32) ;
    Guard (17 <= abpp) ; Guard (nbpp <= 32) }
| { Guard (nbpp == 64) ;
    Guard (33 <= abpp) ; Guard (nbpp <= 64) }

def CatIntEnds nbpp abpp =
  CatIntLow nbpp abpp
| CatIntHigh nbpp abpp

def CatIntFull nbpp abpp =
  CatIntEnds nbpp abpp
| CatIntMid nbpp abpp

def CatReals nbpp abpp =
  { Guard (nbpp == 32) ; Guard (abpp == 32) }
| { Guard (nbpp == 64) ; Guard (abpp == 64) }

def CatComplex nbpp abpp = {
  Guard (nbpp == 64) ;
  Guard (abpp == 64)
}

-- CatParams: formalization of Table A-2(A)
-- DOC: is there a way to do total pattern matching on a sum type?
def CatParams (icat : ICat) (isubcat : ISubCatN) nbands (pvtype : PVType) nbpp abpp =
  {   icat is visible
    | icat is optical ;
      isubcat is default
    | @(isubcat is userdef) ;
      { Guard (nbands == 1) ;
        pvtype is bilevel ;
        Guard (nbpp == 1) ;
        Guard (abpp == 1) }
    | {   Guard (nbands == 1)
        | Guard (nbands == 3) ;
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
    Guard (nbands == 1) ;
      { pvtype is integer ; CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| { icat is infrared ;
    Guard (nbands == 1) ;
      { pvtype is integer ; CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| {   icat is colorPhoto 
    | icat is colorPatch ;
      isubcat is default
    | @(isubcat is userdef) ;
    Guard (nbands == 3) ;
    pvtype is integer ; CatIntEnds nbpp abpp }
| {   icat is rasterMap
    | icat is legends ;
      isubcat is default
    | @(isubcat is userdef) ;
      Guard (nbands == 1)
    | Guard (nbands == 3) ;
    pvtype is integer ; CatIntEnds nbpp abpp }
| { icat is locationGrid ;
      isubcat is easting
    | isubcat is northing
    | isubcat is longitude
    | isubcat is latitude ;
    Guard (nbands == 2) ;
      {   pvtype is integer
        | pvtype is signed ;
        CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| { icat is otherMatrix ;
    isubcat is userdef ;
    Guard (1 <= nbands) ; Guard (nbands <= 9) ;
      { pvtype is complex ; CatComplex nbpp abpp }
    | {   pvtype is integer
        | pvtype is signed ;
        CatIntFull nbpp abpp }
    | { pvtype is real ; CatReals nbpp abpp } }
| {   icat is multiSpectral
    | icat is hyperSpectral ;
      @(isubcat is waveLength)
    | isubcat is default ; -- required for milsamples, but does it match spec?
      { Guard (2 <= nbands) ; Guard (nbands <= 9) }
    | Guard (nbands == 0) ;
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
      { Guard (nbands == 1) ; pvtype is complex ; CatComplex nbpp abpp }
    | {   Guard (nbands == 1)
        | Guard (nbands == 2) ;
          {   pvtype is integer
            | pvtype is signed ;
            CatIntFull nbpp abpp }
        | { pvtype is real ; CatReals nbpp abpp } } }
| {   icat is airWind
    | icat is waterCurrent ;
      isubcat is speed
    | isubcat is direct ;
    Guard (nbands == 2) ;
    pvtype is integer ;
    Guard (nbpp == 8) ;
    Guard (2 <= abpp) ; Guard (abpp <= 8) }
| {   icat is barometric
    | icat is waterDepth ;
    Guard (nbands == 1) ;
    pvtype is integer ;
      CatIntLow nbpp abpp
    | CatIntMid nbpp abpp }
| { icat is elevationModel ;
    Guard (nbands == 1) ;
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
      Guard (nbands == 0) ;
      XBands nbands
    } ;
    no_xband = Guard (nbands != 0)
  } ;

  @num_bands =
    { Guard (nbands != 0) ;
      ^ nbands }
  | { Guard (nbands == 0) ;
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
        Guard (nlutsn > 0) ;
        nelutn = NELutN ;
        lutd_nm = LutdNM nlutsn ;
      } ;
      no_luts = Guard (nlutsn == 0)
    } 
  } ;

  ISync ;

  imode = IMode nbands ;
  nbpr = NBPR ;
  -- TODO: rework to remove negations
  nbpc = NBPC ;
    (Guard (nbpr != 1) | Guard (nbpc != 1))
  | (imode is block | imode is pixel | imode is row);
  -- is R really allowed? Needed by i_3201c.ntf.

  nppbh = NPPBH ;
  Guard (nbpr * nppbh >= n_cols) ;

  nppbv = NPPBV ;
  Guard (nbpc * nppbv >= n_rows) ;

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
      Guard (udidl > 0) ;
      udofl = UDOfl ;
      udid = UDID udidl
    } ;
    empty = Guard (udidl == 0)
  } ;

  ixshdl = IXShDL ;
  Choose {
    ixs = {
      Guard (ixshdl > 0) ;
      ixsofl = IXSOfl ;
      ixshd = IXShD ixshdl
    } ;
    empty = Guard (ixshdl == 0)
  }
}
