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

def IID1 = Many 10 (AlphaNum <| $['_' | ' '])

def IDaTim = PartialDateTime

-- BE: basic encyclopedia
def BE = Many 10 AlphaNum

def OSuffix = Many 5 AlphaNum

def TgtId = block
  be = DefaultSpaces 10 BE
  osuffix = DefaultSpaces 5 OSuffix
  country = DefaultSpaces 2 CountryCode

def IID2 = Many 80 Byte

-- SeqWs: a sequence of P's of size k, padded by spaces to be n bytes
-- def SeqWs k n P = DefaultSpaces n { P ; SeqWs k (n - k) P }
-- DOC: why is this not a valid recursive type?

-- DOC: handling numeric literals

def ISorce = DefaultSpaces 42 (Many 42 Byte)

def NRows = PosNumber 8

def NCols = PosNumber 8

def PVType = First
  integer = @PadMatch 3 ' ' "INT"
  bilevel = @PadMatch 3 ' ' "B"
  signed  = @PadMatch 3 ' ' "SI"
  real    = @PadMatch 3 ' ' "R"
  complex = @PadMatch 3 ' ' "C"

def IRep = First
  monochrome  = PadMatch 8 ' ' "MONO"
  rgb         = PadMatch 8 ' ' "RGB"
  rgblut      = PadMatch 8 ' ' "RGB/LUT"
  multi       = PadMatch 8 ' ' "MULTI"
  nodisplay   = PadMatch 8 ' ' "NODISPLY"
  cartesian   = PadMatch 8 ' ' "NVECTOR"
  polar       = PadMatch 8 ' ' "POLAR"
  sar         = PadMatch 8 ' ' "VPH"
  itur        = PadMatch 8 ' ' "YCbCr601"

def ICat = First
  visible             = @PadMatch 8 ' ' "VIS"
  sideLooking         = @PadMatch 8 ' ' "SL"
  thermalInfrared     = @PadMatch 8 ' ' "TI"
  forwardLooking      = @PadMatch 8 ' ' "FL"
  radar               = @PadMatch 8 ' ' "RD"
  electroOptical      = @PadMatch 8 ' ' "EO"
  optical             = @PadMatch 8 ' ' "OP"
  highResolution      = @PadMatch 8 ' ' "HR"
  hyperSpectral       = @PadMatch 8 ' ' "HS"
  colorPhoto          = @PadMatch 8 ' ' "CP"
  blackWhitePhoto     = @PadMatch 8 ' ' "BP"
  synthApertureRadar  = @PadMatch 8 ' ' "SAR"
  sarRadioHologram    = @PadMatch 8 ' ' "SARIQ"
  infrared            = @PadMatch 8 ' ' "IR"
  multiSpectral       = @PadMatch 8 ' ' "MS"
  fingerprints        = @PadMatch 8 ' ' "FP"
  mri                 = @PadMatch 8 ' ' "MRI"
  xray                = @PadMatch 8 ' ' "XRAY"
  catScans            = @PadMatch 8 ' ' "CAT"
  video               = @PadMatch 8 ' ' "VD"
  barometric          = @PadMatch 8 ' ' "BARO"
  waterCurrent        = @PadMatch 8 ' ' "CURRENT"
  waterDepth          = @PadMatch 8 ' ' "DEPTH"
  airWind             = @PadMatch 8 ' ' "WIND"
  -- geographic products:
  rasterMap           = @PadMatch 8 ' ' "MAP"
  colorPatch          = @PadMatch 8 ' ' "PAT"
  legends             = @PadMatch 8 ' ' "LEG"
  elevationModel      = @PadMatch 8 ' ' "DTEM"
  otherMatrix         = @PadMatch 8 ' ' "MATR"
  locationGrid        = @PadMatch 8 ' ' "LOCG"

def ABPP = BoundedNum 2 1 96

def PJust = First
  leftJust  = @$['L']
  rightJust = @$['R']

def ICords = DefaultSpace (
  First
    utm           = @$['U']
    northernhemi  = @$['N']
    southernhemi  = @$['S']
    geographic    = @$['G']
    decimal       = @$['D']
  )

def LatDeg = block
  sign = Sign
  @whole_digs = Many 2 Digit
  whole = ^ numBase 10 whole_digs
  $['.']
  @frac_digs = Many 3 Digit
  frac = ^ numBase 10 frac_digs
  Guard (whole < 90 || ((whole == 90) && (frac == 0)))


def LongDeg = block
  sign = Sign
  @whole_digs = Many 3 Digit
  whole = ^ numBase 10 whole_digs
  $['.']
  @frac_digs = Many 3 Digit
  frac = ^ numBase 10 frac_digs
  Guard (whole < 180 || ((whole == 180) && (frac == 0)))

def Latitude = block
  digs = PadMany 6 ' ' Numeral
  hemi = First
           north = @$['N']
           south = @$['S']

def Longitude = block
  digs = PadMany 7 ' ' Numeral
  hemi = First
           east = @$['E']
           west = @$['W']

def LatLong = block
  lat = Latitude
  long = Longitude

def UtmZone = block
  zone = BoundedNum 2 1 60

def FiveDigitNum = block
  @v = Many 5 Digit
  ^ numBase 10 v

def PlainUtm = block
  utm = Many 2 Numeral
  easting = Many 6 Numeral
  northing = Many 7 Numeral

def OmitIO lb ub = $[
  lb .. 'H'
| 'J' .. 'N'
| 'P' .. ub
]

def MGRS = block
  zone_num = Many 2 Digit
  zone_band = OmitIO 'C' 'X'
  sq_id =
    block
      col_id = OmitIO 'A' 'Z'
      row_id = OmitIO 'A' 'V'
  easting = Many 5 Numeral
  northing = Many 5 Numeral

def EqLat l0 l1 = {
  Guard (l0.sign == l1.sign) ;
  Guard (l0.whole == l1.whole) ;
  Guard (l0.frac == l1.frac)
}

-- TODO: refine this to allow only rectangles or triangles
--XXX: This can be one single expression
def OrdLong left right =
-- NOTE-MODERN: was nonoverlapping and rewritten with one `Guard`
  Guard (
  ( (left.sign == '-') && (right.sign == '+'))
  || ( (left.sign == right.sign)
     &&
       ( ( (left.sign == '-')
         && (  (left.whole > right.whole)
            || ( (left.whole == right.whole)
               &&
                 (left.frac >= right.frac)
               )
            )
         )
       || ( (left.sign == '+')
          &&
            ( (left.whole < right.whole)
            || ( (left.whole == right.whole) &&
                 (left.frac <= right.frac) )
            )
          )
       )
     )
   )

def IGeoLo = First
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
    }
  lat_long = Many 4 LatLong
  mgrs = Many 4 MGRS
  plain_utm = Many 4 PlainUtm

def NICom = Digit as! uint 64

def IComn n = Many n (Many 80 Byte)

def IC = First
  c1 = @Match "C1"
  c3 = @Match "C3"
  c4 = @Match "C4"
  c5 = @Match "C5"
  c6 = @Match "C6"
  c7 = @Match "C7"
  c8 = @Match "C8"
  i1 = @Match "I1"
  m1 = @Match "M1"
  m3 = @Match "M3"
  m4 = @Match "M4"
  m5 = @Match "M5"
  m6 = @Match "M6"
  m7 = @Match "M7"
  m8 = @Match "M8"
  nc = @Match "NC"
  nm = @Match "NM"

def ComRat (ic : IC) = First

  dim_coding =
    case ic of
      c1, m1 ->
        First
          oned  = @(PadMatch 4 ' ' "1D")
          twods = @(PadMatch 4 ' ' "2DS")
          twodh = @(PadMatch 4 ' ' "2DH")

  quant_tables =
    case ic of
      c3, c5, i1, m3, m5 ->
        block
          $['0']
          img_data_type = BoundedDigit 0 4
          $['.']
          quality_level = BoundedDigit 0 5
          case ic of
            c5, m5     -> Guard (quality_level == 0)
            c3, i1, m3 -> {}

  bits_per_pixel =
    case ic of
      c4, m4 ->
        block
          ones = Digit
          $['.']
          tenths = Digit
          hudredths = Digit

  nominal =
    case ic of
      c8, m8 -> Many 4 Byte


def NBands (irep : IRep) =
  case irep of {
    nodisplay  -> Digit as uint 64;
    monochrome -> IsNum 1 1;
    rgb        -> IsNum 1 3;
    rgblut     -> IsNum 1 1;
    itur       -> IsNum 3 3;
    cartesian  -> Digit as uint 64;
    polar      -> IsNum 1 2;
    sar        -> IsNum 1 1;
    multi      -> (  IsNum 1 0
                  <| BoundedNum 1 2 9);
    _          -> IsNum 1 0 -- NOTE: this case is captured by previous one
  }

def XBands n = BoundedNum 5 10 99999

def IRepBandN = First
  lutBand         = @PadMatch 2 ' ' "LU"
  red             = @PadMatch 2 ' ' "R"
  green           = @PadMatch 2 ' ' "G"
  blue            = @PadMatch 2 ' ' "B"
  monoBand        = @PadMatch 2 ' ' "M"
  luminance       = @PadMatch 2 ' ' "Y"
  chrominanceBlue = @PadMatch 2 ' ' "Cb"
  chrominanceRed  = @PadMatch 2 ' ' "Cr"
  default         = @Spaces 2

-- NOTE-MODERN: changed to `First` to prevent overlapping of `default` and `userdef` with space `0x20`.
def ISubCatN = First
  inphase     = @PadMatch 6 ' ' "I"
  quadrature  = @PadMatch 6 ' ' "Q"
  magnitude   = @PadMatch 6 ' ' "M"
  phase       = @PadMatch 6 ' ' "P"
  speed       = @PadMatch 6 ' ' "SPEED"
  direct      = @PadMatch 6 ' ' "DIRECT"
  easting     = @PadMatch 6 ' ' "CGX"
  northing    = @PadMatch 6 ' ' "CGY"
  longitude   = @PadMatch 6 ' ' "GGX"
  latitude    = @PadMatch 6 ' ' "GGY"
  waveLength  = UnsignedNum 6
  default     = @Spaces 6
  userdef     = Many 6 BCSA

def IFCN = $['N']  -- other values reserved for future use

def ImFltN = Spaces 3 -- reserved for future use

def NELutN = BoundedPos 5 65536

def LutdNM n = Many n Byte

def ISync = $['0'] -- reserved for future use

def IMode nbands =
  block
    $$ = First
            blockMode = @$['B']
            pixel     = @$['P']
            row       = @$['R']
            seq       = @$['S']
    Guard (nbands != 1) <| $$ is blockMode
    -- NOTE-MODERN: Fix using <| to prevent multiple parse

def NBPR = PosQuad

def NBPC = PosQuad

def NPPBH = UpperBounded 4 8192

def NPPBV = UpperBounded 4 8192

def NBPP abpp (ic : IC) = block
  $$ = BoundedPos 2 96
  Guard ($$ >= abpp)
  case ic of { -- NOTE-MODERN: changed to `case` to prevent multiple parses
      c3, c5, i1, m3, m5 -> Guard ($$ == 8 || $$ == 12);
      c1                 -> Guard ($$ == 1);
      c1, m8             -> Guard (1 <= $$ && $$ <= 38);
      c4, c6, c7, c8, m1, m4, m6, m7, nc, nm -> {}
    }

def IDLvl = BoundedNum 3 1 999

-- TODO: this and image display level need to satisfy a global
-- property over all image segments

def IALvl = AttachmentLvl

def IMag = First
  fp =
    block
      $$ = FixedPoint4
      let fplen = length $$.digs + 1 + length $$.radix
      Spaces (4 - fplen)

  frac =
    block
      $['/']
      -- sjw: should this be non-zero?
      $$ = Many (1..3) Digit
      let fplen = length $$ + 1
      Spaces (4 - fplen)

-- encoding of display-dependent parameters (Table A-2)
def DispParams (irep : IRep) (irepband : IRepBandN) nbands (pvtype : PVType) nluts =
  case irep of {
    nodisplay -> {
      case irepband of {
        default ->
          case pvtype of {
            integer , real , complex , bilevel , signed ->
              Guard ((((1 <= nbands) && (nbands <= 9)) || nbands == 0) && (nluts == 0));
            };
        };
      };

    monochrome -> {
      case irepband of {
        lutBand, monoBand, default ->
          case pvtype of {
            integer, real, bilevel ->
              Guard ((nbands == 1) && ((0 <= nluts) && (nluts <= 2)));
          };
        };
      };

    rgb ->
      case irepband of {
        red, green, blue ->
          case pvtype of {
            integer, real -> Guard ((nbands == 3) && (nluts == 0));
          };
      };

    rgblut ->
      case irepband of {
        lutBand ->
          case pvtype of {
            integer, real -> Guard ((nbands == 1) && (nluts == 3));
          };
      };

    itur -> {
      case irepband of {
        luminance, chrominanceBlue, chrominanceRed ->
          case pvtype of {
            integer ->
              Guard ((nbands == 3) && (nluts == 0));
          };
        };
      };

    cartesian -> {
      case irepband of {
        default ->
          case pvtype of {
            integer , real , complex ->
              Guard ((((1 <= nbands) && (nbands <= 9)) || nbands == 0) && (nluts == 0));
            };
        };
      };

    polar -> {
      case irepband of {
        default, monoBand ->
          case pvtype of {
            integer , real , complex ->
              Guard ( nbands == 2 && nluts == 0 );
            };
        };
      };

    sar -> {
      case irepband of {
        default ->
          case pvtype of {
            integer , real , complex ->
              Guard ( nbands == 2 && nluts == 0 );
            };
        };
      };

  multi -> {
    case irepband of {
      default, monoBand, red, green, blue, lutBand ->
        Guard (
          (((2 <= nbands) && (nbands <= 9)) || (nbands == 0))
          &&
          ((0 <= nluts) && (nluts <= 3))
        );
      };
    }
  }

def catIntLow nbpp abpp =
  (nbpp == 8) && (2 <= abpp) && (abpp <= 8)

def catIntMid nbpp abpp =
  ((nbpp == 12) && (8 <= abpp) && (abpp <= 12)) ||
  ((nbpp == 16) && (9 <= abpp) && (abpp <= 16))

def catIntHigh nbpp abpp =
  ((nbpp == 32) && (17 <= abpp) && (abpp <= 32)) ||
  ((nbpp == 64) && (33 <= abpp) && (abpp <= 64))

def catIntEnds nbpp abpp =
  catIntLow nbpp abpp || catIntHigh nbpp abpp

def CatIntEnds nbpp abpp =
  Guard (catIntEnds nbpp abpp)

def CatIntFull nbpp abpp =
  Guard (catIntEnds nbpp abpp || catIntMid nbpp abpp)

def CatReals nbpp abpp =
  Guard (
    ((nbpp == 32) && (abpp == 32)) ||
    ((nbpp == 64) && (abpp == 64))
  )

def CatComplex nbpp abpp =
  Guard (nbpp == 64 && abpp == 64)

-- CatParams: formalization of Table A-2(A)
-- DOC: is there a way to do total pattern matching on a sum type?
def CatParams (icat : ICat) (isubcat : ISubCatN) nbands (pvtype : PVType) nbpp abpp =
  case icat of {
    visible, optical ->
      case isubcat of {
        default, userdef ->
          case pvtype of {
            bilevel -> Guard ((nbands == 1) && (nbpp == 1) && (abpp == 1));
            _ -> {
              Guard ((nbands == 1) || (nbands == 3)) ;
              case pvtype of {
                integer -> CatIntFull nbpp abpp;
                real -> CatReals nbpp abpp
              }
              };
            };
        };

    sideLooking,
    thermalInfrared,
    forwardLooking,
    radar,
    electroOptical,
    highResolution,
    blackWhitePhoto,
    fingerprints,
    video,
    catScans,
    mri,
    xray -> {
      case isubcat of {
        default, userdef -> {
          Guard (nbands == 1) ;
          case pvtype of {
            integer -> CatIntFull nbpp abpp;
            real    -> CatReals nbpp abpp;
          }
        };
      }
      };

    infrared -> {
      Guard (nbands == 1) ;
      case pvtype of {
        integer -> CatIntFull nbpp abpp;
        real    -> CatReals nbpp abpp;
      }
      };

    colorPhoto, colorPatch -> {
      case isubcat of {
        default, userdef -> {
          Guard (nbands == 3) ;
          case pvtype of {
            integer -> CatIntEnds nbpp abpp;
            }
          };
      }
      };

    rasterMap, legends -> {
      case isubcat of {
        default, userdef -> {
          Guard ((nbands == 1) || (nbands == 3)) ;
          case pvtype of {
            integer -> CatIntEnds nbpp abpp;
          }
          };
        }
      };

    locationGrid -> {
      case isubcat of {
        easting, northing, longitude, latitude -> {
          case pvtype of {
            integer, signed -> {
              Guard (nbands == 2) ;
              CatIntFull nbpp abpp
              };
            real -> CatReals nbpp abpp;
          };
        };
      }
      };

    otherMatrix -> {
      case isubcat of {
        userdef -> {
          case pvtype of {
            complex -> {
                Guard ((1 <= nbands) && (nbands <= 9)) ;
                CatComplex nbpp abpp
              };
            integer, signed -> CatIntFull nbpp abpp;
            real -> CatReals nbpp abpp;
          };
          };
      }
      };

    multiSpectral, hyperSpectral -> {
      case isubcat of {
        waveLength, default -> {
          case pvtype of { -- required for milsamples, but does it match spec?
            integer, signed -> {
              Guard (((2 <= nbands) && (nbands <= 9))
                    || (nbands == 0)) ;
              CatIntFull nbpp abpp};
            real -> CatReals nbpp abpp;
            }
          };
        }
      };

    synthApertureRadar, sarRadioHologram ->
      case isubcat of {
        inphase, quadrature, magnitude, phase, default ->
          case pvtype of {
            complex -> { Guard (nbands == 1) ; CatComplex nbpp abpp };
            _ -> {
              Guard ( (nbands == 1) || (nbands == 2) ) ;
              case pvtype of {
                integer, signed -> CatIntFull nbpp abpp;
                real -> CatReals nbpp abpp;
                }
              };
          };
      };

    airWind, waterCurrent ->
      case isubcat of {
        speed, direct -> {
          Guard (nbands == 2) ;
          case pvtype of {
            integer ->
              Guard ((nbpp == 8) && (2 <= abpp) && (abpp <= 8));
            };
          };
        };

    barometric, waterDepth -> {
      Guard (nbands == 1) ;
      case pvtype of {
        integer -> {
            Guard (catIntLow nbpp abpp || catIntMid nbpp abpp)
          };
        };
      };

    elevationModel -> {
      Guard (nbands == 1) ;
      case pvtype of {
        integer, signed -> CatIntFull nbpp abpp;
        real -> CatReals nbpp abpp;
        }
      };
}

-- ISHeader: an image segment header
def ISHeader = {
  @start = Offset;
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
  mb_igeolo = First
                igeolo = { icords is actual ; IGeoLo }
                empty  = icords is default
    ;

  nicom = NICom ;
  i_com_n = IComn nicom ;

  ic = IC ;

  mb_comrat = First
                comrat =
                  case ic of {
                    c1, c3, c4, c5, c6, c8, m1, m3, m4, m5, m8, i1 -> ComRat ic;
                  };

                empty =
                     ic is nc
                  <| ic is nm
    ;

  nbands = NBands irep ;

  xbands = First
    def_xband = {
      Guard (nbands == 0) ;
      XBands nbands
      }
    no_xband = Guard (nbands != 0)
   ;
  @num_bands =
    { Guard (nbands != 0) ;
      ^ nbands }
  <| { Guard (nbands == 0) ;
      @bnds = xbands is def_xband ;
      ^ bnds
    } ;

  bandinfo = Many (num_bands as! uint 64) {
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

    mb_luts = First
                luts = block
                         Guard (nlutsn > 0)
                         nelutn  = NELutN
                         lutd_nm = LutdNM nlutsn

                no_luts = Guard (nlutsn == 0)
        ;
  } ;

  ISync ;

  imode = IMode nbands ;
  nbpr = NBPR ;
  -- TODO: rework to remove negations
  nbpc = NBPC ;
  case imode of {
    blockMode, pixel, row -> {};
    _ -> Guard ((nbpr != 1) || (nbpc != 1));
  };
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

  udid  = UserData 99999;
  ixshd = UserData 99999;
  @end = Offset;
  let bsize_bandinfo =
    for (sz = 0; bi in bandinfo) {
    	sz + (2 + 6 + 1 + 3 + 1
	      + (case bi.mb_luts of { luts _ -> 5 + bi.nlutsn; _ -> 0}))
    };
  let bsize_val =
        2  -- Im ;
      + 10 -- IID1 ;
      + 14 -- IDaTim ;
      + (10 + 5 + 2) -- tgt_id = TgtId ;
      + 80  -- iid2 = IID2 ;
      + bsize_CommonSubheader
      + 1 -- Encryp ;
      + 42 -- ISorce ;
      + 8  -- NRows ;
      + 8  -- NCols ;
      + 3  -- PVType ;
      + 8  -- IRep ;
      + 8  -- ICat ;
      + 2  -- ABPP ;
      + 1  -- PJust ;
      + 1  -- ICords ;
      + (case mb_igeolo of { igeolo _ -> 60; _ -> 0 }) -- mb_igeolo
      + 1  -- NICom ;
      + ((nicom as! uint 64) * 80)  -- IComn nicom ;
      + 2 -- IC
      + (case mb_comrat of { comrat -> 4; _ -> 0 })
      + 1 -- NBands irep ;
      + (case xbands of { def_xband -> 5; _ -> 0 })
      + bsize_bandinfo 
      + 1 -- ISync ;
      + 1 -- IMode nbands ;
      + 4 --  NBPR ;
      + 4 -- NBPC ;
      + 4 -- NPPBH ;
      + 4 -- NPPBV ;
      + 2 -- NBPP abpp ic ;
      + 3 -- IDLvl ;
      + 3 -- IALvl ;
      + 10 -- Location ;
      + 4  -- IMag ;
      + bsize_UserData udid
      + bsize_UserData ixshd ;
  bsize = ^ bsize_val;
  -- Guard (bsize == (end - start))
        
}
