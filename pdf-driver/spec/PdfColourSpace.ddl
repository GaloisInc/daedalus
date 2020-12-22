import PdfValue

def ThreeDCoord = Between "[" "]" {
  xCoord = Number ;
  yCoord = Number ;
  zCoord = Number
}

-- Device color spaces (Sec. 8.6.4):
def DeviceFam = Choose1 {
  deviceGray = "/DeviceGray" ;
  deviceRGB = "/DeviceRGB" ;
  deviceCMYK = "/DeviceCMYK" ;
}

-- CalGrayParialDict: a partial parameter dict for a GalGray space
-- TODO: implement using non-det alternation
def PartialCalGrayDict curWP curBP curGamma curOthers = {
  Choose1 {
    whitePoint = {
      Token "/WhitePoint" ;
      commit ;
      @wp = ThreeDCoord ;
      Guard ((PosNum $$.xCoord) && (PosNum $$.zCoord)) ;
      -- numbers XW and ZW shall be positive
      Guard (wp.y == 1.0) ; -- and YW shall be equal to 1.0
      PartialCalGrayDict (just @wp) curBp curGamma curOthers
    } ;
    blackPoint = {
      Token "/BlackPoint" ;
      commit ;
      @bp = ThreeDCoord ; -- an array of three numbers...
      Guard ((NonNeg bp.x) && (NonNeg bp.y) && (NonNeg bp.z))
      -- All three of these numbers shall be non-negative.
      PartialCalGrayDict curWP (just @bp) curGamma curOthers
    } ;
    gamma = {
      Token "/Gamma" ;
      commit ;
      @g = Number ; -- A number G...
      PosNum $$ ; -- G shall be positive
      ParitalCalGrayDict curWP curBP (just @g) curOthers
    } ;
    other = {
      @nm = Name ;
      @v = DirectObj ; -- TODO: maybe generalize this
      PartialCalGrayDict curWP curBP curGamma (Insert nm v curOthers)
    } ;
    done = {
      whitePoint = curWP is just ; --WhitePoint is required
      blackPoint = case curBP is {
        just curBP0 -> curBP0 ;
        nothing -> { -- default value is [0.0 0.0 0.0]
          xCoord = ^ 0.0 ;
          yCoord = ^ 0.0 ;
          zCoord = ^ 0.0 ;
        } 
      } ;
      gamma = case curGamma is {
        just curGamma0 -> curGamma0 ;
        nothing -> 1 -- Default value: 1
      } ;
      others = ^ curOthers ;
    } ;
  }
}

def CalRBGDict = PartialRGBDict nothing nothing nothing nothing empty 

-- CalGrayDict: 
def CalGrayDict = PartialCalGrayDict nothing nothing nothing empty

def PartialCalRGBDict curWP curBP curGamma curMat curOthers = 
  Fail "PartialRGBDict: not implemented"

-- CIE-based color spaces (Sec. 8.6.5):
def CIEBasedFam DefObj = Choose1 {
  calGray = { -- CalGray colour spaces, Table 62:
    "/CalGray" ; -- the name,
    Between "<<" ">>" CalGrayDict 
  } ;
  calRGB = {
    "/CalRGB" ; -- the name,
    Between "<<" ">>" CalRGBDict
  } ;
  lab = {
    "/Lab" ; -- the name,
    Fail "not implemented: Lab parameter dict"
  } ;
  iccBased = {
    "/ICCBased" ;
    Fail "not implemented: ICC parameter stream"
  } ;
}

-- DEPRECATED

-- Color spaces (Sec. 8.6, Table 61)
def ColourSpace = Choose1 {
  -- Special color spaces:
  -- TODO: correctly parse the parameter for these
  sep = "/Separation" ;
  devN = "/DeviceN" ;
  indexed = "/Indexed" ;
  pattern = "/Pattern" ;
}

