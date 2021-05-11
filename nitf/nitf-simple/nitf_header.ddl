import nitf_lib

-- Wrap a value in a Maybe 

def CallMeMaybe P = { @r = P; ^ just r}

{-- Field parsers --} 

def FHDR = Match "NITF" 

def FVER = Match "02.10" 

-- Valid values 01 .. 99 
def CLEVEL ={ 
  @l = UnsignedNum 2; 
  Guard (l != 0); 
  ^l; 
}

def STYPE = Match "BF01" 

def OSTAID =
  block
    bs = Many 10 BCSA;
    Guard (bs != "0000000000");

def FDT = DateTime

def FTITLE = Many 80 ECSA 

def FSCLAS = Match1 ('T' | 'S' | 'C' | 'R' | 'U')

def Digraph = Many (1..) (Match1 ('A'..'Z'))

-- TODO: Field must contain valid codes per FIPS PUB 10-4, "XN", or "  "   
-- TODO: Field must be set if any of the following are set: FSCODE, 
--       FSREL, FSDCTP, FSDCDT, FSDCXM, FSDG, FSDGDT, FSCLTX, FSCATP, 
--       FSCAUT, FSCRSN, FSSRDT, and FSCTLN
def FSCLSY = Digraph | Match "  " 

-- def DigraphSeq = Choose1 { 
--   { @d = Digraph; 
--     @ds = Many { Match1 ' '; $$ = Digraph}; 
--     Many (Match1 ' '); 
--     END; 
--     ^ just (concat [ [d], ds ]) }; 
--   {Many (Match1 ' '); END; ^ nothing}; 
-- } 


def FSCODE = Many 11 ' ' -- Chunk 11 DigraphSeq 



def FSCTLH = Digraph | Match "  " 

def FSREL = Many 20 ' ' -- Chunk 20 DigraphSeq

-- Unclear how to pad single characters here... 
def FSDCTP = Choose { 
  dd = @Match "DD" ; 
  de = @Match "DE" ; 
  gd = @Match "GD" ; 
  ge = @Match "GE" ; 
  o  = @Match "O " ; 
  x  = @Match "X " ; 
  none = @Match "  " ; 
} 

-- TODO Add proper date parsing 
def Date2 = Many 8 ECSA 

-- padding again 
def FSDCXM =
  [ Match1 ' '; Match1 ' '; Match1 'X'; Match1 ('1' .. '8') ] |
  [ Match1 'X'; Match1 ('1' .. '8'); Match1 ' '; Match1 ' ' ] |
  [ Match1 'X'; Match1 '2'; Match1 '5'; Match1 ('1' .. '9') ]

def FSDG = Match1 ('S' | 'C' | 'R')

def FSDGDT = Many 8 ECSA 

def FSCLTX = Many 43 ECSA 

def FS_declass = { 
  declass_type = FSDCTP; 
  fsdcdt = Choose1 {
    { declass_type is dd; CallMeMaybe Date2 }; 
    { Many 8 (Match1 ' '); ^ nothing }; 
  };
  fsdcxm = Choose1 { 
    { declass_type is x; @f = FSDCXM; ^ just f}; 
    { Many 4 (Match1 ' '); ^ nothing }; 
  }; 
  fsdg = Choose1 { 
    { declass_type is gd | declass_type is ge; 
      @f = FSDG; ^ just f};
    { Match1 ' '; ^ nothing }; 
  }; 
  fsdgdt = Choose1 { 
    { declass_type is gd; @d = Date2; ^ just d}; 
    { Many 8 (Match1 ' '); ^ nothing}; 
  };
  fscltx = Choose1 { 
    { declass_type is de | declass_type is ge; 
      CallMeMaybe (@Many 43 ECSA) }; 
    { Many 43 (Match1 ' '); ^ nothing} 
  }
} 

def FSC_auth = Choose1 { 
  { CallMeMaybe { 
      type = Match1 ('O' | 'D' | 'M');
      auth = Many 40 ECSA; 
    }
  };  
  { Match1 ' '; Many 40 ECSA; ^ nothing}; 
} 

def FSCRSN = Match1 ('A' .. 'G' | ' ')

def FSSRDT = Many 8 ECSA 
 
def FSCTLN = Many 15 ECSA 

def FSCOP = Many 5 BCSN

def FSCPYS = Many 5 BCSN

def ENCRYP = BCSN 

-- The nonstandard_utf8 case is a magic sequence that isn't defined in the 
-- NITF standard, but is handled in the Hammer parser
def FBKGC = 
  Choose1 { 
    nonstandard_utf8 = Many 3 {Match [0xef; 0xbf; 0xbd]; ^ 0xbd} : [uint 8]; -- XXX: This seems wrong.
    normal = Many 3 (Match1 (0x00 .. 0xFF));
  } 

def ONAME = Many 24 ECSA 

def OPHONE = Many 18 ECSA 

def FL = Choose1 { 
  unknown_length = Many 12 (Match1 '9');  
  -- TODO put interval bounds here:
  file_length = UnsignedNum 12 ; 
} 

def HL = UnsignedNum 6 

-- TODO: refactor this into code that computes max for some number of UnsignedNums

def ImgLens = {
  lish = BoundedNum 6 439 999998 ;
  li = BoundedPos 10 9999999999
}

-- GraphLens: lengths of graphic segments:
def GraphLens = {
  lssh = BoundedNum 4 258 9999 ;
  seglen = BoundedPos 6 999999
}

def NUMX = Match "000" 

def LTSH = Many 4 BCSN 
def LT = Many 5 BCSN 

def TextLens = {
  ltsh = BoundedNum 4 282 9999 ;
  lt = BoundedPos 5 999999
}

def DataExtLens = {
  ldsh = BoundedNum 4 200 9999 ;
  ld = BoundedPos 9 999999999
}

def LRESH = Many 4 BCSN 
def LRE = Many 7 BCSN 

def ResExtLens = {
  lresh = BoundedNum 4 200 9999 ;
  lre = BoundedPos 7 9999999
}

def UDHDL = Many 5 BCSN 

def UserData = Choose1 { 
                 none = { Match "00000"; ^ {} } ; 
                 udhd = {@l = UnsignedNum 5 as! uint 64;
                         overflow = Many 3 BCSN; 
                         data = Many (l - 3) UInt8; }; 
               } 


{-- Main header parser --}

def Header = { 
  FHDR; 
  fver = FVER;
  clevel = CLEVEL; 
  STYPE;
  ostaid = OSTAID;
  fdt = FDT;

  -- check that the file was created after epoch
  @ep = Epoch ;
  OrdDate ep fdt.date ;

  -- check that the file was created no later than the current date
  @today = Today ;
  OrdDate fdt.date today ;

  ftitle = FTITLE;
  fsclas = FSCLAS; 
  fsclsy = FSCLSY; 
  fscode = FSCODE;  
  fsctlh = FSCTLH; 
  fsrel = FSREL; 
  
  fs_declass = FS_declass; 

  fsc_auth = FSC_auth; 

  fscrsn = FSCRSN; 
  fssrdt = FSSRDT; 
  fsctln = FSCTLN;
  fscop = FSCOP; 
  fscpys = FSCPYS; 
  encryp = ENCRYP; 
  fbkgc = FBKGC;
  oname = ONAME; 
  ophone = OPHONE; 
  fl = FL; 
  hl = HL; 

  numi = UnsignedNum 3 ;
  li = Many (numi as! uint 64) ImgLens ;

  nums = UnsignedNum 3 ;
  graphlens = Many (nums as! uint 64) GraphLens ;

  NUMX; 
  @numt = UnsignedNum 3 ;
  textlens = Many (numt as! uint 64) TextLens ;

  @numdes = UnsignedNum 3 ;
  dataextlens = Many (numdes as! uint 64) DataExtLens ;

  @numres = UnsignedNum 3 ;
  resextlens = Many (numres as! uint 64) ResExtLens ;

  lre = Many (numres as! uint 64) [LRESH; LRE];
  
  udhd = UserData;
  xhd = UserData; 
}
