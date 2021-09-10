-- Combinator to run a parser on a fixed-size chunk

def Chunk n P =
  block
    @cur  = GetStream
    @this = Take n cur
    @next = Drop n cur
    SetStream this
    $$ = P
    SetStream next

-- A parser that succeeds only if the predicate is true.
def Guard p = p is true

-- library of parsers that are generally useful for NITF

def numBase (base : uint 64) (ds : [ uint 8 ]) =
  for (val = 0; d in ds)
    (val * base + (d as uint 64))

-- def strlen s = for (len = (0 : int); c in s) (len + 1)

-- parsers:

-- Force the parser to backtrack
def MyFail = { Choose {}; }

def Etx = Match1 4

{-- Character sets --}

-- BCS character set
def BCS = Match1 (0x20 .. 0x7E | 0x0C | 0x0D)
def BCSA = Match1 (0x20 .. 0x7E )
def BCSN = Match1 (0x30 .. 0x39 | 0x2B | 0x2D )

-- ECS character set
-- TODO: work out error handling if deprecated ECS codes are used
def ECS = BCS
def ECSA = BCSA

def LowerCase = Match1 ('a' .. 'z')

def UpperCase = Match1 ('A' .. 'Z')

def Alpha = UpperCase | LowerCase

def Numeral = Match1 ('0' .. '9')

def Sign = Match1 ('+' | '-')

def Digit = { @d = Numeral ; ^ d - '0' }

def FixedPoint = block
  digs = Many Digit
  Match1 '.'
  radix = Many Digit


def UnsignedNum digs =
  block
    @ds = Many digs Digit
    ^ for (val = 0; d in ds)
          (val * 10 + (d as uint 64))
--  ^ numBase 10 ds

def NegNum digs =
  block
    Match1 '-'
    @n = UnsignedNum digs
    ^ 0 - n

def SignedNum digs = Choose {
  pos = UnsignedNum (digs + 1) ;
  neg = NegNum digs
}

def BoundedNum digs lb ub =
  block
    $$ = UnsignedNum digs
    Guard (lb <= $$ && $$ <= ub)

def PosNumber digs =
  block
    $$ = UnsignedNum digs
    Guard (1 <= $$)

def IsNum digs v = BoundedNum digs v v

-- DOC: why can't the above bounds check be refactored?

def BoundedDigit lb ub = BoundedNum 1 lb ub

def UpperBoundedDigit ub = BoundedDigit 0 ub as! uint 64

def BoundedPos digs ub = BoundedNum digs 1 ub

def UpperBounded digs ub = BoundedNum digs 0 ub

def PosQuad = BoundedNum 4 1 9999

def LowerBoundedOrZero digs lb =
  block
    $$ = UnsignedNum digs
    Guard ($$ == 0 || lb <= $$)


def Pos = Match1 ('1' .. '9')

def AlphaNum = Alpha | Numeral

-- TODO: replace with specific BCS classes
def Byte = Match1 (0 .. 255)

def Spaces (n : uint 64) = Many n (Match1 ' ')

-- def PadWSpaces n P =
--   Chunk n {$$ = P; Many (Match1 ' '); END}

-- useful for when arr is statically known
-- def padArray n pad arr =
--     concat [ arr, map (x in rangeUp 1 (n - length arr)) pad ]

def PadMatch n pad arr =
  { @Match arr; @Many (n - length arr) (Match1 pad) }

-- FIXME: this assumes P consumes exactly 1 char
def PadMany n pad P =
  block
    $$ = Many (..n) P
    Many (n - length $$) (Match1 pad)

def DefaultByte D P = Choose {
  actual = P ;
  default = @D ;
}

def DefaultSpace P = DefaultByte (Match1 ' ') P

-- TODO: rename
def OrBytes n b P = Choose1 { -- NOTE-MODERN: changed to `Choose1` to prevent multiple parses when overlapping
  actual = P ;
  default = @(Many n (Match b))
}

def DefaultSpaces n P = OrBytes n " " P

def OrHyphens n P = OrBytes n "-" P

def Eq x y = Guard (x == y)

def Lt x y = Guard (x < y)

def Leq x y = (Eq x y) | (Lt x y)   -- XXX: Why not use <= ?

def PartialEq (x : OrBytes) (y : OrBytes) =
  x is default
<| y is default
<| { @x0 = x is actual ;
    @y0 = y is actual ;
    Eq x0 y0
  }

def PartialLt (x : OrBytes) (y : OrBytes) =
  x is default
<| y is default
<| { @x0 = x is actual ;
    @y0 = y is actual ;
    Lt x0 y0
  }

def PartialLeq (x : OrBytes) (y : OrBytes) =
  x is default
<| y is default
<| { @x0 = x is actual ;
     @y0 = y is actual ;
     Leq x0 y0
   }

def Date =
  block
    century = UnsignedNum 2
    year = UnsignedNum 2
    month = BoundedPos 2 12
    day = BoundedPos 2 31

def Epoch : Date =
  block
    century = ^ 19
    year = ^ 70
    month = ^ 1
    day = ^ 1

def Today : Date =
  block
    century = ^ 20
    year = ^ 20
    month = ^ 5
    day = ^ 22

def Time =
  block
    hour = UpperBounded 2 23
    min = UpperBounded 2 59
    second = UpperBounded 2 59

def DateTime =
  block
    date = Date
    time = Time

def PartialDate =
  block
    partCentury = OrHyphens 2 (UnsignedNum 2)
    partYear = OrHyphens 2 (UnsignedNum 2)
    partMonth = OrHyphens 2 (BoundedPos 2 12)
    partDay = OrHyphens 2 (BoundedPos 2 31)

def LiftDate (d : Date) : PartialDate =
  block
    partCentury = ^ {| actual = d.century |}
    partYear = ^ {| actual = d.year |}
    partMonth = ^ {| actual = d.month |}
    partDay = ^ {| actual = d.day |}

def PartialTime =
  block
    partHour = OrHyphens 2 (UpperBounded 2 23)
    partMin = OrHyphens 2 (UpperBounded 2 59)
    partSecond = OrHyphens 2 (UpperBounded 2 59)

def LiftTime (t : Time) : PartialTime =
  block
    partHour = ^ {| actual = t.hour |}
    partMin = ^ {| actual = t.min |}
    partSecond = ^ {| actual = t.second |}

def PartialDateTime =
  block
    partDate = PartialDate
    partTime = PartialTime

def LiftDateTime (dt : DateTime) : PartialDateTime =
  block
    partDate = LiftDate dt.date
    partTime = LiftTime dt.time

def PartialOrdDate (d0 : PartialDate) (d1 : PartialDate) =
  PartialLt d0.partCentury d1.partCentury
<| { PartialEq d0.partCentury d1.partCentury ;
      PartialLt d0.partYear d1.partYear
    <| { PartialEq d0.partYear d1.partYear ;
          PartialLt d0.partMonth d1.partMonth
        <| { PartialEq d0.partMonth d1.partMonth ;
            PartialLeq d0.partDay d1.partDay } } }

def PartialOrdTime (t0 : PartialTime) (t1 : PartialTime) =
   PartialLt t0.partHour t1.partHour
<| { PartialEq t0.partHour t1.partHour ;
     PartialLt t0.partMin t1.partMin
     <| { PartialEq t0.partMin t1.partMin ;
          PartialLeq t0.partSecond t1.partSecond } }

def PartialOrdDateTime (dt0 : PartialDateTime) (dt1 : PartialDateTime) =
  block
    PartialOrdDate dt0.partDate dt1.partDate
    PartialOrdTime dt0.partTime dt1.partTime

-- OrdDate: check that two dates are ordered
def OrdDate (d0 : Date) (d1 : Date) =
  block
    @d0val = LiftDate d0
    @d1val = LiftDate d1
    PartialOrdDate d0val d1val

-- security classifications:
def SecClas = Choose { -- NOTE-MODERN: non-overlapping
  topsecret = @Match1 'T' ;
  secret = @Match1 'S' ;
  confidential = @Match1 'C' ;
  restricted = @Match1 'R' ;
  unclassified = @Match1 'U' ;
}

def CountryCode = Many 2 AlphaNum

-- ClSy: classification system
def ClSy = DefaultSpaces 2 (
  Choose1 { -- NOTE-MODERN: explicitly `Choose1` to prevent overlapping
    nato = @Match "XN" ;
    country = CountryCode ;
  })

-- CodeWords: a space-separated sequence of codewords
def CodeWords = DefaultSpaces 11 {
  first = SecCtrlMarking ;
  rest = Many (..3) {
    Match1 ' ' ;
    SecCtrlMarking
  } ;
  Spaces (11 - (2 + 3 * length rest))
}

-- Security control markings: translated from Table A-4:
def SecCtrlMarking = Choose { -- NOTE-MODERN: non-overlapping, at least not on JITC
  atomal = @Match "AT" ;
  cndwdi = @Match "CN" ;
  copyright = @Match "PX" ;
  cosmic = @Match "CS" ;
  crypto = @Match "CR" ;
  efto = @Match "TX" ;
  formrestData = @Match "RF" ;
  fouo = @Match "FO" ;
  generalService = @Match "GS" ;
  limOffUse = @Match "LU" ;
  limdis = @Match "DS" ;
  nato = @Match "NS" ;
  noContract = @Match "NC" ;
  noncompartment = @Match "NT" ;
  orcon = @Match "OR" ;
  personalData = @Match "IN" ;
  propin = @Match "PI" ;
  restrictedData = @Match "RD" ;
  sao = @Match "SA" ;
  sao1 = @Match "SL" ;
  sao2 = @Match "HA" ;
  sao3 = @Match "HB" ;
  saoSi2 = @Match "SK" ;
  saoSi3 = @Match "HC" ;
  saoSi4 = @Match "HD" ;
  siop = @Match "SH" ;
  siopEsi = @Match "SE" ;
  specialControl = @Match "SC" ;
  specialIntel = @Match "SI" ;
  usOnly = @Match "UO" ;
  warningNotice = @Match "WN" ;
  wnintel = @Match "WI" ;
}

def CtlHandling = DefaultSpaces 2 SecCtrlMarking

def Release = Many 20 (UpperCase | Match1 ' ')

def DeclassificationType = Choose { -- NOTE-MODERN: non-overlapping
  date = @Match "DD" ;
  event = @Match "DE" ;
  datelv = @Match "GD" ;
  eventlv = @Match "GE" ;
  oadr = @Match "O " ;
  exempt = @Match "X " ;
  none = @(Spaces 2)
}

def Declassification = {
  dctp = DeclassificationType ;
  dcdt = Choose { -- NOTE-MODERN: nonoverlapping
    decldate = {
      -- TODO: cleanup parens
      (dctp is date | dctp is datelv) ;
      Date
    } ;
    nodate = {
      ( dctp is event
      | dctp is eventlv
      | dctp is oadr
      | dctp is exempt
      | dctp is none) ;
      Spaces 8
    }
  } ;
  dxcm = Choose { -- NOTE-MODERN: nonoverlapping
    reason = {
      dctp is exempt ;
      Match1 'X' ;
      $$ = PadMany 3 ' ' Digit;
      @v = ^ for (val = 0; d in $$)
                 (val * 10 + (d as uint 64));
		 -- (numBase 10 $$) ;
        { Guard (1   <= v && v <= 8)   }
      | { Guard (251 <= v && v <= 259) }
    } ;
    notexempt = @{
        dctp is date
      | dctp is event
      | dctp is datelv
      | dctp is eventlv
      | dctp is oadr
      | dctp is none ;
      Spaces 4 ;
    }
  } ;
  dg = Choose { -- NOTE-MODERN: nonoverlapping
    actual = {
        dctp is datelv
      | dctp is eventlv ;
      DefaultSpace (
        Choose { -- NOTE-MODERN: nonoverlapping
          secret = @Match1 'S' ;
          confidential = @Match1 'C' ;
          restricted = @Match1 'R' ;
        })
    } ;
    none = @{
        dctp is date
      | dctp is event
      | dctp is oadr
      | dctp is exempt
      | dctp is none ;
      Spaces 1 ;
    }
  } ;
  dgdt = Choose { -- NOTE-MODERN: nonoverlapping
    hasdgdt = {
      dctp is datelv ;
      Date
    } ;
    nodgdt = @{
        dctp is date
      | dctp is event
      | dctp is eventlv
      | dctp is oadr
      | dctp is exempt
      | dctp is none ;
      Spaces 8 ;
    }
  } ;
  cltx = Choose1 { -- NOTE-MODERN: fixed to `Choose1` bc overlapping on SPACE char
    hascltx = {
        dctp is datelv
      | dctp is eventlv ;
      Many 43 ECSA
    } ;
    nocltx = Spaces 43 ;
  }
}

-- authority type:
def ClassificationAuthority = block
  authtp = DefaultSpace (
    Choose { -- NOTE-MODERN: nonoverlapping
      original = @Match1 'O' ;
      derivative = @Match1 'D' ;
      multiple = @Match1 'M' ;
    })
  auth = Many 40 ECSA
  crsn = DefaultSpace (
    Choose { -- NOTE-MODERN: nonoverlapping
      clsrsnA = @Match1 'A' ;
      clsrsnB = @Match1 'B' ;
      clsrsnC = @Match1 'C' ;
      clsrsnD = @Match1 'D' ;
      clsrsnE = @Match1 'E' ;
      clsrsnF = @Match1 'F' ;
      clsrsnG = @Match1 'G' ;
    })

def Security =
  block
    srdt = DefaultSpaces 8 Date
    ctln = DefaultSpaces 15 (Many 15 Digit)

def CommonSubheader =
  block
    clas = SecClas
    clsy = ClSy
    code = CodeWords
    ctlh = CtlHandling
    rel = Release
    decl = Declassification
    clauth = ClassificationAuthority
    sec = Security

def Encryp = Match1 '0'

def AttachmentLvl = UpperBounded 3 998

def Location =
  block
    row = SignedNum 4
    col = SignedNum 4
